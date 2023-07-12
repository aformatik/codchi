{-# LANGUAGE CPP #-}

module Main where

#if defined(mingw32_HOST_OS)
import           Windows                  (runCodchiLIO)
#elif (darwin_HOST_OS)
import           Darwin                   (runCodchiLIO)
#else
import           Linux                    (runCodchiLIO)
#endif

import           Data.List                (maximum, nubBy)
import qualified Data.Text                as Text

import           System.Directory         (createDirectoryIfMissing, doesFileExist)
import           System.Exit              (ExitCode (..))

import           Cleff
import           Cleff.Error

import           Control.Concurrent.Async (concurrently, wait, withAsync)
import           Control.Concurrent.STM   (TChan, dupTChan, readTChan, writeTChan)
import           Control.Monad.Logger     (LogLine, MonadLogger, monadLoggerLog, runFileLoggingT,
                                           runStdoutLoggingT)
import           Dsl
import           Main.Utf8                (withUtf8)
import           Parser                   (Command (..), parseCmd)
import           Types
import           Util

findInstance :: CodchiL :> es => InstanceName -> Eff es (Maybe CodchiInstance)
findInstance name = find ((== name) . _name) <$> listInstances

cli :: [Logger, CodchiL, IOE] :>> es
       => Errors '[NixError, Panic] :>> es
       => Command -> Eff es ()
cli = \case
    CommandStatus            ->
        getStatus >>= \case
            CodchiNotInstalled -> printErrExit (ExitFailure 1) $
                "The codchi controller is not installed. Please run '" <> _APP_NAME <> " start' to install it."
            CodchiInstalled status -> do
                printLnOut $ "Controller Status: " <> show status

                instances <- listInstances
                if not (null instances)
                    then printLnOut $ showTable "\t"
                             $ [ "NAME", "STATUS" ]
                             : [ [getInstanceName _name, show _status] | CodchiInstance{..} <- instances ]
                    else printLnOut "No code machines found!"

    CommandStart            -> do
        let doStart status = do
                printLnOut "Updating controller..."
                let cmd = "cd / && nix run --refresh github:aformatik/codchi#controller-rootfs.passthru.createContents"
                _ <- either (throwError . NixError) return . logTraceShowId "controller update"
                        =<< runCtrlNixCmd True cmd

                printLnOut "Starting controller..."
                start (status == Running)
        getStatus >>= \case
            CodchiInstalled status -> doStart status
            CodchiNotInstalled -> do
                initCtrl
                getStatus >>= \case
                    CodchiNotInstalled -> throwError $ Panic $
                        UnrecoverableError "Couldn't install controller!"
                    CodchiInstalled status -> doStart status

    CommandInstall name flakes -> do
        whenJustM (findInstance name) $ \_ ->
            printErrExit (ExitFailure 1) $
                "Code machine " <> getInstanceName name <> " already exists"

        instanceDir <- getPath DirCtrl $
            fromPathSegments ["instances", getInstanceName name]
        instanceFlake <- getPath DirCtrl $
            fromPathSegments ["instances", getInstanceName name, "flake.nix"]

        driverModule <- getDriverModule

        liftIO $ do
            createDirectoryIfMissing True instanceDir
            writeFileText instanceFlake $ mkNixFlake name (addNamesFromURL flakes) driverModule

        let cmd = "/bin/ctrl-install " <> getInstanceName name

        tarballPath <- either
            (throwError . NixError)
            (rethrowPanic @ParseException . parse_ @StorePath)
                =<< runCtrlNixCmd True cmd

        rootfsFile <- getPath DirCtrl $
            unStorePath tarballPath `joinPaths` fromPathSegments ["rootfs.tar"]

        unlessM (liftIO $ doesFileExist rootfsFile) $
            throwError $ Panic $ NixError $
                "Nix build didn't build " <> toText rootfsFile <> " as expected."

        installRootfs name rootfsFile

        -- printLnOut "Activating system..."
        -- let installCmd = "/nix/var/nix/profiles/system/bin/switch-to-configuration switch"
        -- _ <- either (throwError . NixError) return
        --     =<< runInstanceCmd True name installCmd

        printLnOut "Creating shortcuts..."
        updateShortcuts name =<< getPath DirCtrl =<< getInstanceSWSharePath name

        printLnOut $ "Successfully installed " <> getInstanceName name

    CommandUninstall name   -> do
        whenNothingM_ (findInstance name) $
            printErrExit (ExitFailure 1) $
                "Code machine " <> getInstanceName name <> " does not exists"

        uninstallInstance name
        void $ either (throwError . NixError) return
            =<< runCtrlNixCmd False ("rm -r /instances/" <> getInstanceName name)

    CommandUpdate name -> do
        unlessM isCtrlRunning $
            printErrExit (ExitFailure 1) "Codchi controller is not running..."

        whenNothingM_ (findInstance name) $
            printErrExit (ExitFailure 1) $
                "Code machine " <> getInstanceName name <> " does not exists"

        let cmd = "/bin/ctrl-install " <> getInstanceName name
        printLnOut "Building new system..."
        _ <- either
            (throwError . NixError)
            return
            . logTraceShowId "store path"
                =<< runCtrlNixCmd True cmd

        printLnOut "Updating system..."

        let installCmd = "/nix/var/nix/profiles/system/bin/switch-to-configuration switch"

        _ <- either (throwError . NixError) return
            =<< runInstanceCmd True name installCmd

        printLnOut "Creating shortcuts..."
        updateShortcuts name =<< getPath DirCtrl =<< getInstanceSWSharePath name

        printLnOut "System update successfull!"


    CommandRun showTerm name args    -> do
        unlessM isCtrlRunning $
            printErrExit (ExitFailure 1) "Codchi controller is not running..."

        findInstance name >>= \case
            Nothing -> printErrExit (ExitFailure 1) $
                "Code machine " <> getInstanceName name <> " does not exists"
            Just i -> runInInstance i showTerm args

isCtrlRunning :: CodchiL :> es => Eff es Bool
isCtrlRunning =
    getStatus <&> \case
        CodchiInstalled status -> status == Running
        _                      -> False

-- install :: Codchi :> es => Bool -> Eff es ()
-- install firstInstall = do
--     pass

mkNixFlake :: InstanceName -> [CodchiFlake Text] -> Text -> Text
mkNixFlake instanceName inputs driverModule = unlines
    [ "{"
    , "  description = \"Automatically generated flake for code machine " <>
           getInstanceName instanceName <> ". DO NOT EDIT MANUALLY!\";"
    , "  inputs = {"
    , "    codchi-upstream.url = \"github:aformatik/codchi\";"
    , forEachUnline uniqueInputs $ \fl ->
      "    " <> _flakeMetadata fl <> ".url = \"" <> _flakeURL fl <> "\";"
    -- TODO optionally follow nixpkgs of user flake
    -- , optionalStr (listToMaybe inputs) $ \fl ->
    --   "    nixpkgs.follows = \"" <> _flakeMetadata fl <> "/nixpkgs\";"
    , "    nixpkgs.follows = \"codchi-upstream/nixpkgs\";"
    , "  };"
    , "  outputs = inputs: {"
    , "    nixosConfigurations.default = inputs.nixpkgs.lib.nixosSystem {"
    , "      system = \"x86_64-linux\";"
    -- , "      specialArgs = { inherit inputs; };"
    , "      modules = ["
    , "        { codchi.instance.name = \"" <> getInstanceName instanceName <> "\"; }"
    , "        inputs.codchi-upstream.nixosModules." <> driverModule
    , forEachUnline inputs $ \fl ->
      "        inputs." <> _flakeMetadata fl <> ".nixosModules." <> _moduleName fl
    , "      ];"
    , "    };"
    , "  };"
    , "}"
    ]

    where
        forEachUnline xs f = unlines $ map f xs
        -- optionalStr may f = maybe mempty f may

        uniqueInputs = nubBy (\a b -> _flakeMetadata a == _flakeMetadata b) inputs


showTable :: Text -> [[Text]] -> Text
showTable sep t = unlines (map mkLine t) where
    colWidths = map (maximum . map Text.length)
              $ transpose t

    mkLine = Text.intercalate sep
           . zipWith (`Text.justifyLeft` ' ') colWidths

getInstanceSWSharePath
    :: Errors [NixError, Panic] :>> es
    => CodchiL :> es 
    => InstanceName -> Eff es (Path Rel)
getInstanceSWSharePath name = do
    let cmd = "readlink -f \"/nix/var/nix/profiles/per-instance/" <> getInstanceName name <> "/system/sw/share\""
    either
        (throwError . NixError)
        (rethrowPanic @ParseException . fmap unStorePath . parse_ @StorePath)
            =<< runCtrlNixCmd False cmd



main :: IO ()
main = withUtf8 $ do

    logFile <- runCodchiL $ getPath DirState (fromPathSegments [_APP_NAME <> ".log"])

    let stdoutSink = runStdoutLoggingT . unTChanLoggingT =<< atomically (dupTChan logChan)
        fileSink = runFileLoggingT logFile . unTChanLoggingT =<< atomically (dupTChan logChan)

    withAsync (stdoutSink `concurrently` fileSink) $ \action -> do
        runCodchiL . cli =<< parseCmd
        atomically $ writeTChan logChan Nothing
        void $ wait action

    where
        runCodchiL = runIOE
                  . interpretLogger
                  . pluckEffects @IOE @(Errors '[Panic, DriverException, NixError, UserError])
                  . runCodchiLIO

unTChanLoggingT :: (MonadLogger m, MonadIO m) => TChan (Maybe LogLine) -> m ()
unTChanLoggingT chan = fix $ \loop ->
    atomically (readTChan chan) >>= \case
        Just (loc,src,lvl,msg) -> monadLoggerLog loc src lvl msg >> loop
        Nothing                -> pass

