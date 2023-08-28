{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Codchi where

import Codchi.CLI
import Codchi.Config
import Codchi.Config.IO
import Codchi.Error
import Codchi.Nix
import Codchi.Parser (parse, parse_)
import Codchi.Platform
import Codchi.Types
import qualified Control.Exception.Annotated.UnliftIO as Ann
import Data.List (maximum)
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Development.GitRev (gitHash)
import RIO hiding (atomically, show, unlessM, unlines, whenM)
import UnliftIO.Directory (copyFile, createDirectoryIfMissing, doesFileExist, getCurrentDirectory)

_GIT_COMMIT :: String
_GIT_COMMIT = $(gitHash)

cli :: Command -> RIO Codchi ()
cli CmdStart = do
    let doStart = do
            logInfo "Updating controller..."
            -- we update to the git commit with which the codchi executable was compiled with
            -- to ensure that executable and controller are compatible
            gitCommit <- logTraceId "current git hash" _GIT_COMMIT
            let cmd =
                    Text.intercalate
                        " && "
                        [ "cd /"
                        , "nix run github:aformatik/codchi/"
                            <> toText gitCommit
                            <> "#controller-rootfs.passthru.createContents"
                        , "/bin/ctrl-update-profile"
                        ]
            _ <-
                logTraceId "controller update"
                    =<< runCtrlNixCmd_ StreamStd cmd

            logInfo "Starting controller..."
            controllerStart
    getStatus >>= \case
        CodchiNotInstalled -> do
            controllerInit
            getStatus >>= \case
                CodchiNotInstalled -> Ann.throw (InternalPanic "Couldn't install controller!")
                _ -> doStart
        _ -> doStart
cli CmdStatus =
    getStatus >>= \case
        CodchiNotInstalled ->
            logExit $
                "The codchi controller is not installed. Please run '" <> _APP_NAME <> " start' to install it."
        status -> do
            logInfo $
                "Controller Status: " <> case status of
                    CodchiStopped -> "Stopped"
                    CodchiRunning -> "Running"

            instances <- listMachines
            if not (null instances)
                then
                    logInfo $
                        display $
                            showTable
                                "\t"
                                ( ["NAME", "STATUS"]
                                    : [[i.name.text, show i.status] | i <- instances]
                                )
                else logInfo "No code machines found!"
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
        logExit $
            "Certificate " <> show name.text <> " already exists."

    copyFile absolutePath certFile

    _ <-
        logTraceId "controller update certificates"
            =<< runCtrlNixCmd_ StreamStd "/bin/ctrl-update-certs"

    logInfo $
        "Added certificate " <> show name.text <> " from " <> fromString absolutePath <> " to controller certifacte store."
cli (CmdInit instanceName) =
    modifyConfig $ \cfg ->
        case Map.lookup instanceName.text cfg.instances of
            Just _existing ->
                logExit $
                    "Code machine " <> display instanceName.text <> " already exists."
            Nothing ->
                return (cfg{instances = Map.insert instanceName.text (defaultInstance instanceName) cfg.instances})
cli (CmdAddModule instanceName opts) = modifyInstance instanceName $ \i ->
    case i.modules !? opts.moduleName.text of
        Just m ->
            logExit $
                "Not overwriting existing module "
                    <> display m.name.text
                    <> ". Remove manually with `codchi remove-module`"
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
                        logExit $
                            "Could not parse URI "
                                <> show opts.uri
                                <> ". Allowed formats: http(s), ssh, absolute file path. Specific error: "
                                <> fromString err
                    Right u -> return u

            let modul = Module{uri, moduleType = opts.moduleType, name = opts.moduleName, branchCommit = opts.branchCommit}

            -- automatically try to follow codchis' nixpkgs if not set already
            nixpkgsFollows <-
                if isJust i.nixpkgsFollows
                    then logTraceId "add-module: already set" i.nixpkgsFollows
                    else do
                        follow <- getNixpkgsInput modul
                        case follow of
                            Just (ModuleNixpkgs m) ->
                                logInfo $
                                    "Code machine "
                                        <> display i.name.text
                                        <> " now follows nixpkgs from "
                                        <> display m.text
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
            logExit $
                "Code machine " <> display i.name.text <> " has no module with name " <> show moduleName.text <> "."
        Just _ -> do
            nixpkgsFollows <-
                if i.nixpkgsFollows == Just (ModuleNixpkgs moduleName)
                    then do
                        logWarn $
                            "Code machine " <> show instanceName.text <> " followed " <> display moduleName.text <> "s' nixpkgs. This will now default to codchis' nixpkgs."
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
            logInfo $ "Code machine " <> show instanceName.text <> " now follows codchis' nixpkgs."
            return (i{nixpkgsFollows = Just follows})
        ModuleNixpkgs m ->
            case i.modules !? m.text of
                Nothing ->
                    logExit $
                        "Code machine "
                            <> display instanceName.text
                            <> " has no module with name "
                            <> show m.text
                Just modul -> do
                    nixpkgsFollows <- getNixpkgsInput modul
                    when (isJust nixpkgsFollows) $
                        logInfo $
                            "Code machine "
                                <> display instanceName.text
                                <> " now follows "
                                <> display m.text
                                <> "s' nixpkgs."
                    return (i{nixpkgsFollows})
cli (CmdRebuild instanceName) = do
    cfg <- readConfig
    case cfg.instances !? instanceName.text of
        Nothing ->
            logExit $
                "Code machine "
                    <> display instanceName.text
                    <> " does not exist."
        Just machine -> do
            instanceDir <- getDriverPath DirCtrl (fromList ["instances", instanceName.text])
            instanceFlake <- getDriverPath DirCtrl (fromList ["instances", instanceName.text, "flake.nix"])

            driverModule <- getDriverModule
            follows <- case machine.nixpkgsFollows of
                Just u -> return u
                Nothing -> do
                    logWarn $
                        "Follow not set for "
                            <> display instanceName.text
                            <> ". Defaulting to codchis' nixpkgs."
                    return CodchiNixpkgs

            createDirectoryIfMissing True instanceDir
            writeNixFile instanceFlake (mkNixFlake machine follows driverModule)

            let cmd = "/bin/ctrl-install " <> instanceName.text

            tarballPath <-
                parse_ @StorePath
                    =<< runCtrlNixCmd_ StreamStd cmd

            rootfsFile <- getDriverPath DirCtrl (tarballPath.path </> fromList ["rootfs.tar"])

            info <- findCodeMachine_ instanceName

            case info.status of
                MachineNotInstalled -> do
                    unlessM (doesFileExist rootfsFile) $
                        Ann.throw (InternalNixError $ "Nix build didn't build " <> rootfsFile <> " as expected.")
                    logInfo "Registering with driver..."
                    driverInstallInstance instanceName rootfsFile
                MachineRunning -> do
                    logInfo "Activating system..."
                    let installCmd = "/nix/var/nix/profiles/system/bin/switch-to-configuration switch"
                    runInstanceCmd StreamStd instanceName installCmd >>= \case
                        Left err -> Ann.throw $ InternalNixError $ toString err
                        _ -> pass
                _ -> pass

            logInfo "Creating shortcuts..."
            updateShortcuts instanceName =<< getDriverPath DirCtrl =<< getInstanceSWSharePath instanceName

            logInfo $ "Successfully installed " <> display instanceName.text
cli (CmdRun instanceName showTerm args) = do
    unlessM isCtrlRunning $
        logExit "Codchi controller is not running..."

    findInstance instanceName >>= \case
        Nothing ->
            logExit $
                "Code machine " <> display instanceName.text <> " does not exist."
        Just i -> runInInstance i showTerm args
cli (CmdUninstall instanceName) = modifyConfig $ \c ->
    case c.instances !? instanceName.text of
        Nothing -> do
            logExit $
                "Code machine " <> display instanceName.text <> " does not exist."
        Just _ -> do
            logInfo $
                "Do you really want to uninstall " <> display instanceName.text <> "?. THIS WILL DELETE ALL OF YOUR DATA!"
            logInfo "Type 'yes' to confirm!"
            answer <- getLine
            if answer /= "yes"
                then logExit "Aborted uninstall."
                else do
                    instanceStatus <- (.status) <$> findCodeMachine_ instanceName
                    when (instanceStatus /= MachineNotInstalled) $
                        driverUninstallInstance instanceName instanceStatus
                    _ <-
                        runCtrlNixCmd_ StreamIgnore $
                            "rm -r /instances/" <> instanceName.text <> " || true"
                    return c{instances = Map.delete instanceName.text c.instances}

findInstance :: MonadCodchi m => CodchiName -> m (Maybe InstanceConfig)
findInstance name = do
    cfg <- readConfig
    return (cfg.instances !? name.text)

modifyInstance :: CodchiName -> (InstanceConfig -> RIO Codchi InstanceConfig) -> RIO Codchi ()
modifyInstance name f = modifyConfig $ \cfg ->
    case Map.lookup name.text cfg.instances of
        Just i -> do
            i' <- f i
            return (cfg{instances = Map.insert name.text i' cfg.instances})
        Nothing ->
            logExit $ "Code machine " <> display name.text <> " does not exist."

getNixpkgsInput :: Module -> RIO Codchi (Maybe NixpkgsFollows)
getNixpkgsInput m = do
    let cmd = "nix flake metadata --no-write-lock-file --json '" <> toFlakeUrl m <> "' | jq '.locks.nodes | has(\"nixpkgs\")'"
    output <-
        logTraceId "getNixpkgsInput"
            . fmap Text.strip
            =<< runCtrlNixCmd StreamIgnore cmd
    case output of
        Right "true" -> do
            return (Just $ ModuleNixpkgs m.name)
        _ -> return Nothing

isCtrlRunning :: MonadCodchi m => m Bool
isCtrlRunning = (== CodchiRunning) <$> getStatus

showTable :: Text -> [[Text]] -> Text
showTable sep t = unlines (map mkLine t)
  where
    colWidths =
        map (maximum . map Text.length) $
            transpose t

    mkLine =
        Text.intercalate sep
            . zipWith (`Text.justifyLeft` ' ') colWidths

getInstanceSWSharePath :: MonadCodchi m => CodchiName -> m (Path Rel)
getInstanceSWSharePath name = do
    let cmd = "readlink -f \"/nix/var/nix/profiles/per-instance/" <> name.text <> "/system/sw/share\""
    fmap (.path) . parse_ @StorePath
        =<< runCtrlNixCmd_ StreamIgnore cmd
