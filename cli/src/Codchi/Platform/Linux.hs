{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant fmap" #-}

module Codchi.Platform.Linux where

import Codchi.Config
import Codchi.Config.IO (readConfig)
import Codchi.Error
import Codchi.Platform.CodchiMonad
import Codchi.Types
import qualified Control.Exception.Annotated.UnliftIO as Ann
import qualified Data.ByteString as BS
import Data.ByteString.Builder.Extra (defaultChunkSize)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import RIO (RIO, async, catch, hClose, logError, throwIO)
import qualified RIO.Map as Map
import System.Process.Typed (ExitCode (..), ProcessConfig, readProcess_, runProcess_, shell)
import qualified System.Process.Typed as Proc
import UnliftIO.Directory

_IDMAP :: String
_IDMAP =
    toString $
        unlines
            [ "uid 0 0"
            , "gid 0 0"
            , "uid $(id -u) 1000"
            , "gid $(id -g) 100"
            ]

instance MonadCodchi (RIO Codchi) where
    driverMeta =
        pure $ DriverMeta{moduleName = "lxd"}

    getDriverPath dirType subDir = do
        fp <-
            logTraceId "create-dir"
                =<< case dirType of
                    DirCtrl -> getXdgDirectory XdgData _APP_NAME
                    DirState -> do
                        xdgState <- lookupEnv "XDG_STATE_HOME"
                        xdgHome <- getHomeDirectory
                        let stateDir = fromMaybe (xdgHome <> "/.local/state") xdgState
                        return $ stateDir <> "/" <> _APP_NAME
                    DirConfig -> getXdgDirectory XdgConfig _APP_NAME
        createDirectoryIfMissing True fp
        return $ fp <> "/" <> toString (toUnixPath subDir)

    getStatus = do
        (out, _) <- readProcess_ (shell "lxc list --format csv -c ns")
        out
            & decodeUtf8
            & lines
            & fmap (Text.split (== ','))
            & find ((== Just _CONTROLLER) . listToMaybe)
            & \case
                Just [_, "RUNNING"] -> return CodchiRunning
                Just [_, "STOPPED"] -> return CodchiStopped
                _ -> return CodchiNotInstalled

    listMachines = do
        (out, _) <- readProcess_ (shell "lxc list --format csv -c ns")
        let lxdInstances =
                out
                    & decodeUtf8
                    & lines
                    & fmap (Text.split (== ','))
                    & mapMaybe
                        ( \case
                            [lxdName, status] ->
                                Text.stripPrefix _INSTANCE_PREFIX lxdName
                                    <&> ( \name -> case status of
                                            "RUNNING" -> (name, (name, MachineRunning))
                                            "STOPPED" -> (name, (name, MachineStopped))
                                            _ -> (name, (name, MachineNotInstalled))
                                        )
                            _ -> Nothing
                        )
                    & Map.fromList
        codeMachines <- (.instances) <$> readConfig
        return $
            Map.elems $
                Map.mergeWithKey matched (fmap onlyInLXD) (fmap onlyInCodchi) lxdInstances codeMachines
      where
        matched :: Text -> (Text, MachineStatus) -> InstanceConfig -> Maybe CodeMachine
        matched name (_, status) _machine = Just $ CodeMachine{name = CodchiName name, status}

        onlyInLXD :: (Text, MachineStatus) -> CodeMachine
        onlyInLXD (name, _) = CodeMachine{name = CodchiName name, status = MachineOrphaned}

        onlyInCodchi :: InstanceConfig -> CodeMachine
        onlyInCodchi cfg = CodeMachine{name = cfg.name, status = MachineNotInstalled}

    getControllerPath _ = return $ Left "Adding file modules under Linux is not supported in Codchi."

    controllerInit = do
        let cmds =
                [ "lxc image import $(nix build .#lxd-ctrl-rootfs --no-link --print-out-paths)/controller.tar.gz --alias codchi-controller"
                , "lxc init codchi-controller codchi-controller"
                , "lxc image delete codchi-controller"
                , "mkdir -p $XDG_DATA_HOME/codchi/{nix,instances,instance-state}"
                , "lxc config set codchi-controller security.nesting=true"
                , "lxc config device add codchi-controller nix disk \"source=$XDG_DATA_HOME/codchi/nix\" path=/nix"
                , "lxc config device add codchi-controller instances disk \"source=$XDG_DATA_HOME/codchi/instances\" path=/instances"
                , "lxc config device add codchi-controller instance-state disk \"source=$XDG_DATA_HOME/codchi/instance-state\" path=/instance-state"
                , "printf \"" <> _IDMAP <> "\" | lxc config set codchi-controller raw.idmap -" -- not sure if this is needed
                ]
        forM_ cmds (runProcess_ . shell)
            `catch` ( \case
                        ExitFailure _ -> runProcess_ "lxc image delete -q codchi-controller; lxc delete -qf codchi-controller; true"
                        _ -> pass
                    )

    controllerStart = do
        status <- getStatus
        when (status == CodchiStopped) $
            runProcess_ (shell "lxc start codchi-controller")

    -- runProcess_ (controllerCmd "nix shell nixpkgs#lf -c lf")

    runCtrlNixCmd streamLog cmd = do
        controllerStart
        let rp
                | streamLog == StreamStd = readProcessWith putText putText
                | otherwise = readProcessWith (const pass) (const pass)
        result <- rp $ controllerCmd cmd

        case result of
            Left err ->
                case Text.splitOn "error: " err of
                    [_, e] | not (Text.null e) -> return $ Left $ toString e
                    _ -> Ann.throw $ InternalNixError $ toString err
            Right out -> return $ Right out

    driverInstallInstance name rootfs = do
        let lxdName = _INSTANCE_PREFIX <> toString name.text
            codchiName = toString name.text
            mountCtrlDir devName src dest =
                "lxc config device add "
                    <> lxdName
                    <> " "
                    <> devName
                    <> " disk \"source=$XDG_DATA_HOME/codchi"
                    <> src
                    <> "\" path="
                    <> dest
        let cmds =
                [ "lxc image import \"" <> rootfs <> "\" --alias " <> lxdName
                , "mkdir -p $XDG_DATA_HOME/codchi/instance-state/" <> codchiName
                , "lxc init " <> lxdName <> " " <> lxdName
                , "lxc image delete " <> lxdName
                , "lxc config set " <> lxdName <> " security.nesting=true"
                , mountCtrlDir "nix-store" "/nix/store" "/nix/store"
                , mountCtrlDir "nix-daemon" "/nix/var/nix/daemon-socket" "/nix/var/nix/daemon-socket"
                , mountCtrlDir "nix-profile" ("/nix/var/nix/profiles/per-instance/" <> codchiName) "/nix/var/nix/profiles"
                , mountCtrlDir "nix-all-profiles" "/nix/var/nix/profiles" "/nix/var/nix/profiles/global"
                , mountCtrlDir "nix-db" "/nix/var/nix/db" "/nix/var/nix/db"
                , mountCtrlDir "home" ("/instance-state/" <> codchiName) "/home"
                , "printf \"" <> _IDMAP <> "\" | lxc config set " <> lxdName <> " raw.idmap -"
                ]
        putTextLn $ "Installing " <> show name.text <> " from " <> toText rootfs
        forM_ cmds (runProcess_ . shell)
            `catch` ( \case
                        ExitFailure _ -> driverUninstallInstance name MachineNotInstalled
                        _ -> pass
                    )
    updateShortcuts _name _path = logError "updateShortcuts is not implemented yet"
    driverUninstallInstance name _status = do
        let lxdName = _INSTANCE_PREFIX <> toString name.text
        mapM_
            (runProcess_ . shell)
            [ "lxc delete -qf " <> lxdName <> " || true"
            , "lxc image delete -q " <> lxdName <> " || true"
            , "rm -rf $XDG_DATA_HOME/codchi/instance-state/" <> toString name.text
            ]

    runInInstance i showTerm cmd = do
        instanceStatus <- (.status) <$> findCodeMachine_ i.name
        when (instanceStatus == MachineStopped) $ do
            mapM_
                (runProcess_ . shell)
                [ "lxc start " <> _INSTANCE_PREFIX <> toString i.name.text
                , "lxc exec " <> _INSTANCE_PREFIX <> toString i.name.text <> " -- bash -c 'until [ -f /run/current-system/sw/bin/bash ]; do sleep .1; done'"
                ]
        let wrapIO
                | showTerm =
                    Proc.setStdin (Proc.useHandleOpen stdin)
                        . Proc.setStdout (Proc.useHandleOpen stdout)
                        . Proc.setStderr (Proc.useHandleOpen stderr)
                | otherwise =
                    Proc.setStdin Proc.nullStream
                        . Proc.setStdout Proc.nullStream
                        . Proc.setStderr Proc.nullStream

        runProcess_ $
            wrapIO $
                lxdCmd (_INSTANCE_PREFIX <> i.name.text) "nixos" $
                    case cmd of
                        [] -> "bash -l"
                        args -> unwords args
    runInstanceCmd streamLog name cmd = do
        instanceStatus <- (.status) <$> findCodeMachine_ name
        when (instanceStatus == MachineStopped) $ do
            mapM_
                (runProcess_ . shell)
                [ "lxc start " <> _INSTANCE_PREFIX <> toString name.text
                , "lxc exec " <> _INSTANCE_PREFIX <> toString name.text <> " -- bash -c 'until [ -f /run/current-system/sw/bin/bash ]; do sleep .1; done'"
                ]
        let rp
                | streamLog == StreamStd = readProcessWith putText putText
                | otherwise = readProcessWith (const pass) (const pass)
        result <- rp $ lxdCmd name.text "root" cmd

        case result of
            Left err ->
                case Text.splitOn "error: " err of
                    [_, e] | not (Text.null e) -> return $ Left e
                    _ -> Ann.throw $ InternalNixError $ toString err
            Right out -> return $ Right out

instance IsString (Path x) where
    fromString = mkUnixPath . toText

controllerCmd :: Text -> ProcessConfig () () ()
controllerCmd = lxdCmd _CONTROLLER "root"

lxdCmd :: Text -> Text -> Text -> ProcessConfig () () ()
lxdCmd name user cmd =
    shellCmd "lxc" $
        ["exec", name, "--", "su", "-l", user, "-c"] -- <> (["-x" | logLevel <= LevelDebug]) FIXME
            <> ["'" <> cmd <> "'"]

shellCmd :: Text -> [Text] -> ProcessConfig () () ()
shellCmd cmd args =
    -- logTraceId "shellCmd" $ FIXME
    Proc.shell $ toString $ unwords $ cmd : args

shellProc :: Text -> [Text] -> ProcessConfig () () ()
shellProc cmd args =
    -- logTraceId "shellProc" $ FIXME
    Proc.proc (toString cmd) (map toString args)

readProcessWith ::
    MonadCodchi m =>
    (Text -> IO ()) ->
    (Text -> IO ()) ->
    ProcessConfig () () () ->
    m (Either Text Text)
readProcessWith outLogger errLogger proc = do
    proc' <-
        Proc.setStdout (loggingTextOutput outLogger)
            . Proc.setStderr (loggingTextOutput errLogger)
            <$> logTraceId "readProcess" proc
    let collect p =
            atomically $
                (,,)
                    <$> Proc.waitExitCodeSTM p
                    <*> Proc.getStdout p
                    <*> Proc.getStderr p
    (exitCode, out, err) <-
        logTraceId "readProcess result"
            =<< Proc.withProcessTerm proc' collect
    case exitCode of
        ExitSuccess ->
            logTraceId "splitWinWSL" $ Right $ toStrict out
        ExitFailure _ ->
            logTraceId "splitWinWSL" $ Left $ toStrict err

loggingTextOutput :: (Text -> IO ()) -> Proc.StreamSpec 'Proc.STOutput (STM LText)
loggingTextOutput logger = Proc.mkPipeStreamSpec $
    \pc h -> do
        mvar <- newEmptyTMVarIO

        void $ async $ do
            let loop front = do
                    bs <- BS.hGetSome h defaultChunkSize
                    let txt = decodeUtf8 $ toLazy bs
                    if BS.null bs
                        then atomically $ putTMVar mvar $ Right $ LText.fromChunks $ front []
                        else do
                            logger txt
                            loop $ front . (txt :)
            loop id `catch` \e -> do
                atomically $ void $ tryPutTMVar mvar $ Left $ Proc.ByteStringOutputException e pc
                throwIO e

        return (either throwSTM return =<< readTMVar mvar, hClose h)
