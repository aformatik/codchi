{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Codchi.Platform.Windows where

#ifdef mingw32_HOST_OS
import Graphics.Win32 hiding (try)
import System.Win32.Shell
import System.Win32.Shortcut
#endif

import Codchi.Config
import Codchi.Config.IO (readConfig)
import Codchi.Error
import Codchi.Parser
import Codchi.Platform.CodchiMonad
import Codchi.Platform.Windows.Internal
import Codchi.Types
import Control.Concurrent.STM (dupTChan, newTChan, tryReadTChan, writeTChan)
import qualified Control.Exception.Annotated.UnliftIO as Ann
import qualified Data.Attoparsec.Text as P
import qualified Data.ByteString as BS
import Data.ByteString.Builder.Extra (defaultChunkSize)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.List (stripPrefix)
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import RIO (LogLevel (..), MonadUnliftIO (..), RIO, async, catch, display, finally, hClose, logGeneric, logInfo, logWarn, threadDelay, throwIO, try)
import qualified RIO.Map as Map
import System.IO.Error (isPermissionError)
import qualified System.IO.Utf8 as Utf8
import System.Process.Typed (ExitCode (..), ProcessConfig)
import qualified System.Process.Typed as Proc
import UnliftIO.Directory

data WSLDistro = WSLDistro
    { name :: !Text
    , status :: !MachineStatus
    , isWSL2 :: !Bool
    }
    deriving (Show)

type IsRunning = Bool

data WinCodchiStatus
    = WSLNotInstalledOrNeedsUpdate
    | ControllerNotInstalled
    | ControllerInstalled IsRunning

instance Parseable [WSLDistro] where
    -- drop table header
    parser = drop 1 <$> p
      where
        p = P.sepBy' lineP P.endOfLine <* P.endOfLine
        lineP =
            WSLDistro
                <$> nameP
                <*> isRunningP
                <*> versionP
                <* P.skipWhile (not . P.isEndOfLine)
        nameP =
            P.skipWhile (\x -> isSpace x || x == '*')
                *> P.takeWhile1 (not . isSpace)
        isRunningP =
            P.skipWhile isSpace
                *> ( (\s -> if s == "Running" then MachineRunning else MachineStopped)
                        <$> P.takeWhile1 (not . isSpace)
                   )
        versionP =
            P.skipWhile isSpace
                *> ((== "2") <$> P.takeWhile1 (not . isSpace))

data Shell = Windows | InWSL deriving (Eq, Show)

-- \| exit failure
newtype ShellException
    = ShellException Text
    deriving (Show)
    deriving anyclass (Exception)

-- | Somethings wrong with controller
newtype ControllerException
    = ControllerException String
    deriving (Show)
    deriving anyclass (Exception)

instance MonadCodchi (RIO Codchi) where
    driverMeta =
        pure $ DriverMeta{moduleName = "wsl"}

    getStatus =
        getControllerStatus >>= \case
            WSLNotInstalledOrNeedsUpdate ->
                Ann.throw $
                    InternalDriverError $
                        "WSL is not installed or needs an update.\r\n"
                            <> "Please run 'wsl --install --web-download --no-distribution' or "
                            <> "download the latest version from 'https://github.com/microsoft/WSL/releases'.\r\n"
                            <> "This will require admin privileges and also restarting your computer."
            ControllerNotInstalled -> return CodchiNotInstalled
            ControllerInstalled isRunning -> return $ if isRunning then CodchiRunning else CodchiStopped
    listMachines = do
        wslDistros <- listWSLDistros
        let wslInstances =
                Map.fromList
                    [ (instanceName, wslDistro)
                    | wslDistro <- wslDistros
                    , instanceName <-
                        maybeToList $
                            Text.stripPrefix _INSTANCE_PREFIX wslDistro.name
                    ]
        codeMachines <- (.instances) <$> readConfig
        return $
            Map.elems $
                Map.mergeWithKey matched (fmap onlyInWSL) (fmap onlyInCodchi) wslInstances codeMachines
      where
        matched :: Text -> WSLDistro -> InstanceConfig -> Maybe CodeMachine
        matched name distro _machine =
            Just $
                CodeMachine{name = CodchiName name, status = distro.status}

        onlyInWSL :: WSLDistro -> CodeMachine
        onlyInWSL distro = CodeMachine{name = CodchiName distro.name, status = MachineOrphaned}

        onlyInCodchi :: InstanceConfig -> CodeMachine
        onlyInCodchi cfg = CodeMachine{name = cfg.name, status = MachineNotInstalled}

    -- case find ((== name) . (.name)) instances of
    --     Nothing -> return (CodchiInstance{name, status = NotInstalled})
    --     Just i -> return i

    controllerInit =
        liftIO (flip findFile "codchi\\controller.tar.gz" =<< getXdgDirectoryList XdgDataDirs) >>= \case
            Nothing ->
                Ann.throw $ InternalPanic "Couldn't find root filesystem for Codchi controller"
            Just rootfs -> do
                ctrlDir <- getOrCreateXdgDir XdgCache (mkNTPath (_CONTROLLER_DIR :: Text))
                ctrlFile <-
                    logTraceId "controller dir" $
                        toNTPath $
                            ctrlDir </> fromList ["controller.tar.gz"]
                copyFile rootfs (toString ctrlFile)

                let wslImport =
                        void $
                            readProcess_ $
                                wsl'exeCmd
                                    [ "--import"
                                    , _CONTROLLER
                                    , toNTPath ctrlDir
                                    , ctrlFile
                                    ]
                    cleanup = removeFile $ toString ctrlFile

                wslImport `finally` cleanup
    controllerStart = mainLoop

    runCtrlNixCmd streamLog cmd = do
        let printWSL Win _ = pass
            printWSL WSL txt = putText txt
            rp
                | streamLog == StreamStd = readProcessWith printWSL printWSL
                | otherwise = readProcess
        result <- rp $ controllerCmd cmd

        case result of
            Left err ->
                case Text.splitOn "error: " err of
                    [_, e] | not (Text.null e) -> return $ Left $ toString e
                    _ -> Ann.throw $ ShellException err
            Right out -> return $ Right out
    runInstanceCmd streamLog inst cmd = do
        let printWSL Win _ = pass
            printWSL WSL txt = putText txt
            rp
                | streamLog == StreamStd = readProcessWith printWSL printWSL
                | otherwise = readProcess
        result <- rp $ wslCmd inst.withPrefix cmd

        case result of
            Left err -> return $ Left err
            Right out -> return $ Right out
    driverInstallInstance name rootfsPath = do
        instanceDir <-
            getOrCreateXdgDir XdgCache $
                mkNTPath ("instances" :: Text) </> mkNTPath name.text

        putTextLn $ "Installing " <> show name.text <> " from " <> toText rootfsPath

        void $
            readProcess_ $
                wsl'exeCmd
                    [ "--import"
                    , name.withPrefix
                    , toNTPath instanceDir
                    , toText rootfsPath
                    ]
    driverUninstallInstance name _status = do
        putTextLn $ "Uninstalling " <> name.text

        _ <-
            readProcess_ $
                wsl'exeCmd ["--unregister", name.withPrefix]

        startMenu <- getFolderPath cSIDL_PROGRAMS
        let instanceFolder = startMenu </> fromList [_APP_NAME <> " - " <> name.text]
        retryOnPermissionErrors 100 $
            removePathForcibly (toString $ toNTPath instanceFolder)
        liftIO refreshIconCache

    runInInstance i showTerm args = do
        liftIO $ do
            consoleWindow <- getConsoleWindow
            case consoleWindow of
                Just hwnd | not showTerm -> failIfFalse_ "hide console window" $ showWindow hwnd sW_HIDE
                _ -> pass
        let wrapIO
                | showTerm =
                    Proc.setStdin (Proc.useHandleOpen stdin)
                        . Proc.setStdout (Proc.useHandleOpen stdout)
                        . Proc.setStderr (Proc.useHandleOpen stderr)
                | otherwise =
                    Proc.setStdin Proc.nullStream
                        . Proc.setStdout Proc.nullStream
                        . Proc.setStderr Proc.nullStream

        Proc.runProcess_ $
            wrapIO $
                wsl'exeCmd $
                    [ "-d"
                    , i.name.withPrefix
                    , "--shell-type"
                    , "login"
                    ]
                        <> args
    updateShortcuts name swSharePath = do
        icosFolder <-
            toString . toNTPath
                <$> getOrCreateXdgDir XdgCache (fromList ["icos", name.text])

        -- cleanup start menu entries
        mapM_ (removeFile . (\f -> icosFolder <> "\\" <> f)) =<< listDirectory icosFolder

        desktopEntries <-
            let parseDesktopEntryAndIco app = do
                    de <-
                        parse @(DesktopEntry ()) . decodeUtf8
                            <$> readFileBS (swSharePath <> "\\codchi\\applications\\" <> toString app <> ".desktop")

                    case de of
                        Left err ->
                            Ann.throw $
                                InternalPanic $
                                    "Could not parse desktop entry for " <> toString app <> ":\n" <> err
                        Right desktopEntry -> do
                            let iconInCtrl = swSharePath <> "\\codchi\\icos\\" <> toString app <> ".ico"
                                iconInWin = icosFolder <> "\\" <> toString desktopEntry.name <> ".ico"
                            icon <-
                                doesFileExist iconInCtrl >>= \case
                                    True -> do
                                        copyFile iconInCtrl iconInWin
                                        return $ Just iconInWin
                                    False -> do
                                        logWarn $ "Could not find .ico for " <> display app
                                        return Nothing
                            return $ desktopEntry{icon}
             in mapM parseDesktopEntryAndIco
                    . mapMaybe (Text.stripSuffix ".desktop" . toText)
                    =<< listDirectory (swSharePath <> "\\codchi\\applications")

        startMenuFolder <- liftIO $ do
            startMenu <- getFolderPath cSIDL_PROGRAMS
            let instanceFolder = startMenu </> fromList [_APP_NAME <> " - " <> name.text]
            createDirectoryIfMissing True (toString $ toNTPath instanceFolder)
            return $ toString $ toNTPath instanceFolder

        -- cleanup start menu entries
        mapM_ (removeFile . (\f -> startMenuFolder <> "\\" <> f)) =<< listDirectory startMenuFolder

        -- create shortcuts
        -- currentDir <- getCurrentDirectory
        homeDir <- getHomeDirectory
        either (Ann.throw . InternalPanic . show) return
            =<< liftIO
                ( runExceptT $ do
                    ExceptT initialize
                    forM_ desktopEntries $ \desktopEntry -> do
                        let exec = toString
                                    . unwords
                                    . filter (not . ("%" `Text.isPrefixOf`))
                                    . words
                                    $ desktopEntry.exec
                            lnk =
                                Shortcut
                                    { targetPath = _APP_NAME <> ".exe"
                                    , arguments =
                                        String.unwords $
                                            ["run"]
                                                <> ["--no-terminal" | not desktopEntry.isTerminal]
                                                <> [ toString name.text
                                                   , "--"
                                                   , exec
                                                   ]
                                    , workingDirectory = homeDir
                                    , showCmd = if desktopEntry.isTerminal then ShowNormal else ShowMinimized
                                    , description = ""
                                    , iconLocation = (fromMaybe "" desktopEntry.icon, 0)
                                    , hotkey = 0
                                    }
                            lnkPath = startMenuFolder <> "\\" <> toString desktopEntry.name <> ".lnk"
                        ExceptT $ writeShortcut lnk lnkPath
                    liftIO uninitialize
                )
        liftIO refreshIconCache

    getDriverPath dirType s =
        toString . toNTPath . (</> s)
            <$> case dirType of
                DirCtrl -> do
                    let dir = getWSLInstanceDir _CONTROLLER
                    createDirectoryIfMissing True $ toString $ toNTPath dir
                    return dir
                DirState -> getOrCreateXdgDir XdgCache emptyPath
                DirConfig -> getOrCreateXdgDir XdgConfig emptyPath
    getControllerPath path =
        case "C:\\" `stripPrefix` path of
            Just p -> return (Right $ fromList ["mnt", "c"] </> mkNTPath p)
            Nothing -> return (Left "Please provide an absolute path located on C:\\")

mainLoop :: forall m. MonadCodchi m => m ()
mainLoop = do
    pulseaudioLogFile <- getDriverPath DirState $ fromList ["pulseaudio.log"]
    loglvl <- (.logLevel) <$> ask
    let pulseaudioLogLevel = case loglvl of
            LevelError -> 0 :: Int
            LevelWarn -> 1
            LevelDebug -> 4
            _ -> 3
    vcxsrvLogFile <- getDriverPath DirState $ fromList ["vcxsrv.log"]
    let vcxsrvLogLevel = case loglvl of
            LevelError -> 0 :: Int
            LevelWarn -> 1
            LevelDebug -> 3
            _ -> 2

    cancelChan <- atomically newTChan

    logInfo "Waking up WSL..."
    void $ runProcessSilent $ controllerCmd "true"
    wslVmId <-
        logTraceId "wslVmId"
            =<< maybe (Ann.throw $ InternalPanic "Can't retrieve WSL's VM ID...") return
            =<< liftIO findWslVmId

    let subprog name cmd args = do
            myCancelChan <- atomically $ dupTChan cancelChan
            void $
                runProcessSilent $
                    shellProc "taskkill.exe" ["/F", "/IM", name]
            let doLog lvl _ = logGeneric name lvl . display . Text.stripEnd
            fix $ \loop -> do
                runProcessWith (doLog LevelInfo) (doLog LevelWarn) (shellProc cmd args)
                whenNothingM_ (atomically $ tryReadTChan myCancelChan) $ do
                    logWarn $ display name <> " exited unexpectedly. Restarting..."
                    threadDelay 1_000_000
                    loop

    void $ do
        winFolder <- toNTPath <$> getFolderPath cSIDL_PROGRAM_FILESx86
        async $
            subprog
                "codchi_pulseaudio.exe"
                (winFolder <> "\\PulseAudio\\bin\\codchi_pulseaudio.exe")
                [ "--log-target=file:" <> toText pulseaudioLogFile
                , "--log-time"
                , "--log-level=" <> show pulseaudioLogLevel
                , "--disallow-exit"
                , "--disallow-module-loading"
                , -- , "--system"
                  "--exit-idle-time=-1"
                ]
    void $ do
        winFolder <- toNTPath <$> getFolderPath cSIDL_PROGRAM_FILES
        async $
            subprog
                "codchi_vcxsrv.exe"
                (winFolder <> "\\VcXsrv\\codchi_vcxsrv.exe")
                [ "-ac" -- disable access control
                , "-noreset" -- dont restart after last client exits
                , "-wgl" -- native opengl
                , "-compositewm" -- previews for windows
                , "-notrayicon"
                , "-dpi"
                , "auto"
                , "-multiwindow" -- seamless mode
                , "-clipboard"
                , "-noprimary"
                , "-logfile"
                , toText vcxsrvLogFile
                , "-logverbose"
                , show vcxsrvLogLevel
                , "-vmid"
                , "{" <> toText wslVmId <> "}"
                , "-vsockport"
                , "6000"
                ]
    -- https://sourceforge.net/p/vcxsrv/discussion/986201/thread/1ab552d067/

    void $ async $ do
        myCancelChan <- atomically $ dupTChan cancelChan
        void $ readProcess $ controllerCmd "pkill nix-daemon || true"
        let doLog lvl _ = logGeneric "controller" lvl . display . Text.stripEnd
        fix $ \loop -> do
            runProcessWith (doLog LevelInfo) (doLog LevelWarn) $
                Proc.setStdin Proc.closed $
                    controllerCmd "/bin/ctrl-serve"
            whenNothingM_ (atomically $ tryReadTChan myCancelChan) $ do
                doLog LevelWarn Win ("Codchi controller exited unexpectedly (please see logs). Restarting..." :: Text)
                loop

    runWinLoop $ do
        putTextLn "Stopping controller..."
        atomically $ writeTChan cancelChan ()
        void $ runProcessSilent $ controllerCmd "pkill nix-daemon || true"
        -- void $ runProcessSilent $ shellProc "taskkill.exe" [ "/F", "/IM", "codchi_pulseaudio.exe" ]
        void $ runProcessSilent $ shellProc "taskkill.exe" ["/F", "/IM", "codchi_vcxsrv.exe"]

-- retryDuring :: MonadIO m => Integer -> Int -> m (Maybe e) -> m (Maybe e)
-- retryDuring timeFrame maxTries act
--     | maxTries < 1  = pure Nothing
--     | otherwise     = loop 1 . round =<< liftIO getPOSIXTime
--     where
--         loop n firstTryTime = do
--             res <- act
--             curTime <- round <$> liftIO getPOSIXTime
--             case res of
--                 Nothing -> return Nothing
--                 Just err
--                     | curTime - firstTryTime <= timeFrame -> if n < maxTries
--                                                                 then loop (n+1) firstTryTime
--                                                                 else return $ Just err
--                     | otherwise -> loop 0 curTime

getOrCreateXdgDir :: MonadLogger e m => XdgDirectory -> Path Rel -> m (Path Abs)
getOrCreateXdgDir xdg dir = do
    path <- mkNTPath <$> getXdgDirectory xdg _APP_NAME
    let innerPath = path </> dir
    fp <- logTraceId "create-dir" (toString $ toNTPath innerPath)
    createDirectoryIfMissing True fp
    return innerPath

-- instance IsString (Path t) where
--     fromString = mkNTPath . toText

controllerCmd :: Text -> ProcessConfig () () ()
controllerCmd = wslCmd _CONTROLLER

wslCmd :: Text -> Text -> ProcessConfig () () ()
wslCmd name cmd =
    wsl'exeCmd $
        [ "-d"
        , name
        , "--shell-type"
        , "login"
        , "--user"
        , "root"
        , -- , "--cd", "/root"
          "bash"
        , "-ic"
        ]
            -- <> (["-x" | logLevel <= LevelDebug]) FIXME
            <> [ show $ "printf " <> _MAGIC_UTF8_SEQ <> " | tee /dev/stderr ; " <> cmd
               ]

wsl'exeCmd :: [Text] -> ProcessConfig () () ()
wsl'exeCmd = shellCmd "wsl.exe"

shellCmd :: Text -> [Text] -> ProcessConfig () () ()
shellCmd cmd args =
    -- logTraceId "shellCmd" $ FIXME
    Proc.shell $
        toString $
            unwords $
                cmd : args

shellProc :: Text -> [Text] -> ProcessConfig () () ()
shellProc cmd args =
    -- logTraceId "shellProc" $ FIXME
    Proc.proc (toString cmd) (map toString args)

listWSLDistros :: MonadCodchi m => m [WSLDistro]
listWSLDistros = parseProcess_ $ wsl'exeCmd ["-l", "-v"]

runProcessSilent :: MonadIO m => ProcessConfig stdin stdout stderr -> m ExitCode
runProcessSilent =
    Proc.runProcess
        . Proc.setStdout Proc.nullStream
        . Proc.setStderr Proc.nullStream

runProcessWith ::
    MonadUnliftIO m =>
    (OutputType -> Text -> m ()) ->
    (OutputType -> Text -> m ()) ->
    ProcessConfig () () () ->
    m ()
runProcessWith outLogger errLogger proc = do
    let proc' =
            proc
                & Proc.setStderr Proc.createPipe
                & Proc.setStdout Proc.createPipe
    Proc.withProcessTerm proc' $ \p -> do
        let send logger h = loop Win
              where
                loop prevType = do
                    bs <- liftIO $ BS.hGetSome h defaultChunkSize
                    let txt = decodeUtf16And8 $ toLazy bs
                        fixEOL
                            | prevType == WSL = Text.replace "\n" "\r\n"
                            | otherwise = id
                    unless (BS.null bs) $
                        case map fixEOL $ Text.splitOn _MAGIC_UTF8_SEQ txt of
                            [win, wsl] -> do
                                void $ logger Win win
                                void $ logger WSL wsl
                                loop WSL
                            _ -> do
                                void $ logger prevType txt
                                loop prevType
        void $ async $ send outLogger (Proc.getStdout p)
        void $ async $ send errLogger (Proc.getStderr p)
        void $ Proc.waitExitCode p

data OutputType = Win | WSL deriving (Eq)

readProcessWith ::
    MonadCodchi m =>
    (OutputType -> Text -> IO ()) ->
    (OutputType -> Text -> IO ()) ->
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
        splitWinWSL txt = case Text.splitOn _MAGIC_UTF8_SEQ txt of
            [_, wsl] | not (Text.null wsl) -> wsl
            _ -> txt
    (exitCode, out, err) <-
        logTraceId "readProcess result"
            =<< Proc.withProcessTerm proc' collect
    case exitCode of
        ExitSuccess ->
            logTraceId "splitWinWSL" $ Right $ splitWinWSL $ toStrict out
        ExitFailure _ ->
            logTraceId "splitWinWSL" $ Left $ splitWinWSL $ toStrict err

loggingTextOutput :: (OutputType -> Text -> IO ()) -> Proc.StreamSpec 'Proc.STOutput (STM LText)
loggingTextOutput logger = Proc.mkPipeStreamSpec loggingTextOutputFromHandle
  where
    loggingTextOutputFromHandle :: ProcessConfig () () () -> Handle -> IO (STM LText, IO ())
    loggingTextOutputFromHandle pc h = Utf8.withHandle h $ do
        mvar <- newEmptyTMVarIO

        void $ async $ do
            let loop prevType front = do
                    bs <- BS.hGetSome h defaultChunkSize
                    let txt = decodeUtf16And8 $ toLazy bs
                        fixEOL
                            | prevType == WSL = Text.replace "\n" "\r\n"
                            | otherwise = id
                    if BS.null bs
                        then atomically $ putTMVar mvar $ Right $ LText.fromChunks $ front []
                        else case map fixEOL $ Text.splitOn _MAGIC_UTF8_SEQ txt of
                            [win, wsl] -> do
                                logger Win win
                                logger WSL wsl
                                loop WSL $ front . (wsl :) . (win :)
                            _ -> do
                                logger prevType txt
                                loop prevType $ front . (txt :)
            loop Win id `catch` \e -> do
                atomically $ void $ tryPutTMVar mvar $ Left $ Proc.ByteStringOutputException e pc
                throwIO e

        return (either throwSTM return =<< readTMVar mvar, hClose h)

-- catchIf
--     :: (MonadUnliftIO m, Exception e)
--     => (e -> Bool) -> m a -> (e -> m a) -> m a
-- catchIf f a b = catch a (\e -> if f e then b e else throwIO e)

readProcess :: MonadCodchi m => ProcessConfig () () () -> m (Either Text Text)
readProcess = readProcessWith (\_ _ -> pass) (\_ _ -> pass)

decodeUtf16And8 :: BL.ByteString -> Text
decodeUtf16And8 = decodeUtf8 . BL.filter (/= 0)

readProcess_ :: MonadCodchi m => ProcessConfig () () () -> m Text
readProcess_ = either (Ann.throw . ShellException) return <=< readProcess

parseProcess_ :: (MonadCodchi m, Parseable a) => ProcessConfig () () () -> m a
parseProcess_ prc = parse_ =<< readProcess_ prc

getControllerStatus :: MonadCodchi m => m WinCodchiStatus
getControllerStatus =
    readProcess "wsl.exe --version" >>= \case
        -- WSL is not activated or needs an update (--version is only available
        -- in later versions)
        Left _ -> return WSLNotInstalledOrNeedsUpdate
        Right _ ->
            readProcess "wsl.exe -l -v" >>= \case
                Right out -> do
                    instances <- parse_ @[WSLDistro] out
                    case find ((== _CONTROLLER) . (.name)) instances of
                        Nothing -> return ControllerNotInstalled
                        Just distro ->
                            return $
                                ControllerInstalled $
                                    distro.status == MachineRunning

                -- WSL outputs an error message to stdout if no distribution is
                -- installed... We assume that if there is another error, there
                -- will be an error message
                Left err | Text.null err -> return ControllerNotInstalled
                Left err -> Ann.throw $ InternalPanic $ toString err

getWSLInstanceDir :: Text -> Path Abs
getWSLInstanceDir instanceName = fromList ["\\\\wsl$", instanceName]

_MAGIC_UTF8_SEQ :: IsString s => s
_MAGIC_UTF8_SEQ = "this_is_for_recognizing_utf8"

-- splitMagicUTF8Seq txt = case Text.splitOn _MAGIC_UTF8_SEQ txt of
--     [win, wsl] -> (win, wsl)
--     _ | _MAGIC_UTF8_SEQ `Text.isInfixOf` txt -> ("", txt)

retryOnPermissionErrors :: (MonadLogger e m, MonadUnliftIO m) => Int -> m a -> m a
retryOnPermissionErrors maxRetries io = do
    res <- try io
    case res of
        Right a -> return a
        Left ex
            | isPermissionError ex && maxRetries > 1 -> do
                retries' <-
                    logTrace "retryOnPermissionErrors" (show @Text (ex, maxRetries)) $
                        maxRetries - 1
                threadDelay ((maxRetries - retries') * 200)
                retryOnPermissionErrors retries' io
            | otherwise -> Ann.throw $ InternalDriverError $ "retryOnPermissionError: " <> displayException ex
