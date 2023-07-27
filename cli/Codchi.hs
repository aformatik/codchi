{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

#if defined(mingw32_HOST_OS)
import Codchi.Platform.Windows (runCodchiLIO)
#elif (darwin_HOST_OS)
import Codchi.Platform.Darwin (runCodchiLIO)
#else
import Codchi.Platform.Linux (runCodchiLIO)
#endif

import Cleff
import Cleff.Error
import Codchi.CLI
import Codchi.Config
import Codchi.Dsl
import Codchi.Effects
import Codchi.Nix
import Codchi.Parser (parse)
import Codchi.Types
import Control.Concurrent.Async (concurrently, wait, withAsync)
import Control.Concurrent.STM (TChan, dupTChan, readTChan, writeTChan)
import Control.Monad.Logger (LogLine, MonadLogger, monadLoggerLog, runFileLoggingT, runStdoutLoggingT)
import Data.List (maximum)
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Main.Utf8 (withUtf8)
import System.Exit (ExitCode (..))
import UnliftIO.Directory (copyFile, createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import Development.GitRev (gitHash)

_GIT_COMMIT :: String
_GIT_COMMIT = logTraceShowId "current git hash" $(gitHash)

cli ::
    [Logger, CodchiL, IOE] :>> es =>
    Errors '[NixError, Panic] :>> es =>
    Command ->
    Eff es ()
cli CmdStart = do
    let doStart = do
            printLnOut "Updating controller..."
            -- we update to the git commit with which the codchi executable was compiled with
            -- to ensure that executable and controller are compatible
            let cmd =
                    Text.intercalate
                        " && "
                        [ "cd /"
                        , "nix run github:aformatik/codchi/"
                            <> toText _GIT_COMMIT
                            <> "#controller-rootfs.passthru.createContents"
                        , "/bin/ctrl-update-profile"
                        ]
            _ <-
                either (throwError . NixError) return . logTraceShowId "controller update"
                    =<< runCtrlNixCmd True cmd

            printLnOut "Starting controller..."
            controllerStart
    getStatus >>= \case
        CodchiInstalled _ -> doStart
        CodchiNotInstalled -> do
            controllerInit
            getStatus >>= \case
                CodchiNotInstalled -> throwError (Panic $ UnrecoverableError "Couldn't install controller!")
                CodchiInstalled _ -> doStart
cli CmdStatus =
    getStatus >>= \case
        CodchiNotInstalled ->
            printErrExit (ExitFailure 1) $
                "The codchi controller is not installed. Please run '" <> _APP_NAME <> " start' to install it."
        CodchiInstalled status -> do
            printLnOut $ "Controller Status: " <> show status

            instances <- mapM (getInstanceInfo . (.name)) . toList . (.instances) =<< readConfig
            if not (null instances)
                then
                    printLnOut $
                        showTable
                            "\t"
                            ( ["NAME", "STATUS"]
                                : [[i.name.text, show i.status] | i <- instances]
                            )
                else printLnOut "No code machines found!"
cli (CmdAddCa name path) = do
    absolutePath <-
        if "." `isPrefixOf` path
            then do
                dir <- getCurrentDirectory
                return (dir <> "/" <> path)
            else return path

    certDir <- getDriverPath DirCtrl (fromList ["etc", "ssl", "certs", "custom"])
    createDirectoryIfMissing True certDir

    let certFile = certDir <> "/" <> toString name.text <> ".crt"

    whenM (doesFileExist certFile) $
        printErrExit (ExitFailure 1) $
            "Certificate " <> show name.text <> " already exists."

    copyFile absolutePath certFile

    _ <-
        either (throwError . NixError) return . logTraceShowId "controller update certificates"
            =<< runCtrlNixCmd True "/bin/ctrl-update-certs"

    printLnOut $
        "Added certificate " <> show name.text <> " from " <> toText absolutePath <> " to controller certifacte store."
cli (CmdInit instanceName) =
    modifyConfig $ \cfg ->
        case Map.lookup instanceName.text cfg.instances of
            Just _existing ->
                printErrExit
                    (ExitFailure 1)
                    ("Code machine " <> instanceName.text <> " already exists.")
            Nothing ->
                return (cfg{instances = Map.insert instanceName.text (defaultInstance instanceName) cfg.instances})
cli (CmdAddModule instanceName opts) = modifyInstance instanceName $ \i ->
    case i.modules !? opts.moduleName.text of
        Just m ->
            printErrExit (ExitFailure 1) $
                "Not overwriting existing module " <> m.name.text <> ". Remove manually with `codchi remove-module`"
        Nothing -> do
            uri <- do
                let remoteParsed = GitModule <$> parse opts.uri
                localParsed <- fmap LocalModule <$> getControllerPath (toString opts.uri)
                let parsed =
                        -- specify parsing order based on heuristics, because only
                        -- the error of the last parser is kept
                        if "C:" `Text.isPrefixOf` opts.uri || "." `Text.isPrefixOf` opts.uri
                            then remoteParsed <> localParsed
                            else localParsed <> remoteParsed
                case parsed of
                    Left err ->
                        printErrExit (ExitFailure 1) $
                            "Could not parse URI "
                                <> show opts.uri
                                <> ". Allowed formats: http(s), ssh, absolute file path. Specific error: "
                                <> err
                    Right u -> return u

            let modul = Module{uri, moduleType = opts.moduleType, name = opts.moduleName, branchCommit = opts.branchCommit}

            -- automatically try to follow codchis' nixpkgs if not set already
            nixpkgsFollows <-
                if isJust i.nixpkgsFollows
                    then return $ logTraceShowId "add-module: already set" i.nixpkgsFollows
                    else do
                        follow <- getNixpkgsInput modul
                        case follow of
                            Just (ModuleNixpkgs m) ->
                                printLnOut $ "Code machine " <> i.name.text <> " now follows nixpkgs from " <> m.text
                            _ -> pass
                        return follow

            return
                ( i
                    { modules = Map.insert opts.moduleName.text modul i.modules
                    , nixpkgsFollows
                    }
                )
cli (CmdRemoveModule instanceName moduleName) = modifyInstance instanceName $ \i ->
    case i.modules !? moduleName.text of
        Nothing ->
            printErrExit (ExitFailure 1) $
                "Code machine " <> i.name.text <> " has no module with name " <> show moduleName.text <> "."
        Just _ -> do
            nixpkgsFollows <-
                if i.nixpkgsFollows == Just (ModuleNixpkgs moduleName)
                    then do
                        logWarn $
                            "Code machine " <> show instanceName.text <> " followed " <> moduleName.text <> "s' nixpkgs. This will now default to codchis' nixpkgs."
                        return Nothing
                    else return i.nixpkgsFollows
            return
                i
                    { nixpkgsFollows
                    , modules = Map.delete moduleName.text i.modules
                    }
cli (CmdSetFollows instanceName follows) = modifyInstance instanceName $ \i ->
    case follows of
        CodchiNixpkgs -> do
            printLnOut $ "Code machine " <> show instanceName.text <> " now follows codchis' nixpkgs."
            return (i{nixpkgsFollows = Just follows})
        ModuleNixpkgs m ->
            case i.modules !? m.text of
                Nothing ->
                    printErrExit (ExitFailure 1) $
                        "Code machine " <> instanceName.text <> " has no module with name " <> show m.text
                Just modul -> do
                    nixpkgsFollows <- getNixpkgsInput modul
                    when (isJust nixpkgsFollows) $
                        printLnOut ("Code machine " <> instanceName.text <> " now follows " <> m.text <> "s' nixpkgs.")
                    return (i{nixpkgsFollows})
cli (CmdRebuild instanceName) = do
    cfg <- readConfig
    case cfg.instances !? instanceName.text of
        Nothing ->
            printErrExit (ExitFailure 1) $
                "Code machine " <> instanceName.text <> " does not exist."
        Just machine -> do
            instanceDir <- getDriverPath DirCtrl (fromList ["instances", instanceName.text])
            instanceFlake <- getDriverPath DirCtrl (fromList ["instances", instanceName.text, "flake.nix"])

            driverModule <- getDriverModule
            follows <- case machine.nixpkgsFollows of
                Just u -> return u
                Nothing -> do
                    logWarn $
                        "Follow not set for " <> instanceName.text <> ". Defaulting to codchis' nixpkgs."
                    return CodchiNixpkgs

            createDirectoryIfMissing True instanceDir
            writeNixFile instanceFlake (mkNixFlake machine follows driverModule)

            let cmd = "/bin/ctrl-install " <> instanceName.text

            tarballPath <-
                either
                    (throwError . NixError)
                    (rethrowPanic @ParseException . parse_ @StorePath)
                    =<< runCtrlNixCmd True cmd

            rootfsFile <- getDriverPath DirCtrl (tarballPath.path </> fromList ["rootfs.tar"])

            info <- getInstanceInfo instanceName

            case info.status of
                NotInstalled -> do
                    unlessM (doesFileExist rootfsFile) $
                        throwError (Panic $ NixError $ "Nix build didn't build " <> toText rootfsFile <> " as expected.")
                    printLnOut "Registering with driver..."
                    driverInstallInstance instanceName rootfsFile
                Running -> do
                    printLnOut "Activating system..."
                    let installCmd = "/nix/var/nix/profiles/system/bin/switch-to-configuration switch"
                    void $
                        either (throwError . NixError) return =<< runInstanceCmd True instanceName installCmd
                Stopped -> pass

            printLnOut "Creating shortcuts..."
            updateShortcuts instanceName =<< getDriverPath DirCtrl =<< getInstanceSWSharePath instanceName

            printLnOut $ "Successfully installed " <> instanceName.text
cli (CmdRun instanceName showTerm args) = do
    unlessM isCtrlRunning $
        printErrExit (ExitFailure 1) "Codchi controller is not running..."

    findInstance instanceName >>= \case
        Nothing ->
            printErrExit (ExitFailure 1) $
                "Code machine " <> instanceName.text <> " does not exist."
        Just i -> runInInstance i showTerm args
cli (CmdUninstall instanceName) = modifyConfig $ \c ->
    case c.instances !? instanceName.text of
        Nothing -> do
            printErrExit (ExitFailure 1) $
                "Code machine " <> instanceName.text <> " does not exist."
        Just _ -> do
            printLnOut $
                "Do you really want to uninstall " <> instanceName.text <> "?. THIS WILL DELETE ALL OF YOUR DATA!"
            printLnOut "Type 'yes' to confirm!"
            answer <- getLine
            if answer /= "yes"
                then printErrExit (ExitFailure 1) "Aborted uninstall."
                else do
                    instanceStatus <- getInstanceInfo instanceName <&> (.status)
                    when (instanceStatus /= NotInstalled) $ 
                        driverUninstallInstance instanceName instanceStatus
                    void $
                        either (throwError . NixError) return
                            =<< runCtrlNixCmd False ("rm -r /instances/" <> instanceName.text <> " || true")
                    return c{instances = Map.delete instanceName.text c.instances}

findInstance :: (CodchiL :> es, IOE :> es) => CodchiName -> Eff es (Maybe InstanceConfig)
findInstance name = do
    cfg <- readConfig
    return (cfg.instances !? name.text)

modifyInstance ::
    [CodchiL, IOE, Logger] :>> es =>
    CodchiName ->
    (InstanceConfig -> Eff es InstanceConfig) ->
    Eff es ()
modifyInstance name f = modifyConfig $ \cfg ->
    case Map.lookup name.text cfg.instances of
        Just i -> do
            i' <- f i
            return (cfg{instances = Map.insert name.text i' cfg.instances})
        Nothing ->
            printErrExit (ExitFailure 1) $ "Code machine " <> name.text <> " does not exist."

getNixpkgsInput :: (CodchiL :> es) => Module -> Eff es (Maybe NixpkgsFollows)
getNixpkgsInput m = do
    let cmd = "nix flake metadata --no-write-lock-file --json '" <> toFlakeUrl m <> "' | jq '.locks.nodes | has(\"nixpkgs\")'"
    output <- logTraceShowId "getNixpkgsInput" . fmap Text.strip <$> runCtrlNixCmd False cmd
    case output of
        Right "true" -> do
            return (Just $ ModuleNixpkgs m.name)
        _ -> return Nothing

isCtrlRunning :: CodchiL :> es => Eff es Bool
isCtrlRunning =
    getStatus <&> \case
        CodchiInstalled status -> status == Running
        _ -> False

showTable :: Text -> [[Text]] -> Text
showTable sep t = unlines (map mkLine t)
  where
    colWidths =
        map (maximum . map Text.length) $
            transpose t

    mkLine =
        Text.intercalate sep
            . zipWith (`Text.justifyLeft` ' ') colWidths

getInstanceSWSharePath ::
    Errors [NixError, Panic] :>> es =>
    CodchiL :> es =>
    CodchiName ->
    Eff es (Path Rel)
getInstanceSWSharePath name = do
    let cmd = "readlink -f \"/nix/var/nix/profiles/per-instance/" <> name.text <> "/system/sw/share\""
    either
        (throwError . NixError)
        (rethrowPanic @ParseException . fmap (.path) . parse_ @StorePath)
        =<< runCtrlNixCmd False cmd

main :: IO ()
main = withUtf8 $ do
    logFile <- runCodchiL $ getDriverPath DirState (fromList [_APP_NAME <> ".log"])

    let stdoutSink = runStdoutLoggingT . unTChanLoggingT =<< atomically (dupTChan logChan)
        fileSink = runFileLoggingT logFile . unTChanLoggingT =<< atomically (dupTChan logChan)

    withAsync (stdoutSink `concurrently` fileSink) $ \action -> do
        runCodchiL . cli =<< parseCmd
        atomically $ writeTChan logChan Nothing
        void $ wait action
  where
    runCodchiL =
        runIOE
            . interpretLogger
            . pluckEffects @IOE @(Errors '[Panic, DriverException, NixError, UserError])
            . runCodchiLIO

unTChanLoggingT :: (MonadLogger m, MonadIO m) => TChan (Maybe LogLine) -> m ()
unTChanLoggingT chan = fix $ \loop ->
    atomically (readTChan chan) >>= \case
        Just (loc, src, lvl, msg) -> monadLoggerLog loc src lvl msg >> loop
        Nothing -> pass
