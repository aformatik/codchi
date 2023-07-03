{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# HLINT ignore "Use unwords" #-}

module Windows
    ( refreshIconCache
    , runDevevIO
    ) where

#ifdef mingw32_HOST_OS
import           Graphics.Win32
import           System.Win32.Console.CtrlHandler
import           System.Win32.DLL                 (getModuleHandle)
import           System.Win32.Registry
import           System.Win32.Shell
import           System.Win32.Shortcut
#else
import           Foreign                          (ForeignPtr, Ptr, nullPtr)
import           Foreign.C                        (CInt, CIntPtr, CWchar)
#endif


import           Devenv
import           Types
import           Util

import           Cleff                            hiding (send)
import           Cleff.Error


import qualified Data.Attoparsec.Text             as P
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isSpace)
import qualified Data.Text                        as Text
import qualified Data.Text.Lazy                   as LText

import           Control.Concurrent               (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM           (dupTChan, newTChan, tryReadTChan, writeTChan)
import           Control.Monad.Logger             (LogLevel (..), logWithoutLoc)
import           Data.ByteString.Builder.Extra    (defaultChunkSize)
import qualified System.IO.Utf8                   as Utf8
import qualified System.Process.Typed             as Proc
import           System.Process.Typed             (ExitCode (..), ProcessConfig)
import           UnliftIO                         (bracket, catch, hClose, throwIO)
import           UnliftIO.Directory

data WSLInstance = WSLInstance
    { _wslName   :: !Text
    , _wslStatus :: !InstanceStatus
    , _isWSL2    :: !Bool
    }
  deriving (Show)

instance Parseable [WSLInstance] where
    -- drop table header
    parser = drop 1 <$> p where
        p = P.sepBy' lineP P.endOfLine <* P.endOfLine
        lineP = WSLInstance
             <$> nameP
             <*> isRunningP
             <*> versionP
             <*  P.skipWhile (not . P.isEndOfLine)
        nameP = P.skipWhile (\x -> isSpace x || x == '*')
              *> P.takeWhile1 (not . isSpace)
        isRunningP = P.skipWhile isSpace
                   *> ((\s -> if s == "Running" then Running else Stopped)
                          <$> P.takeWhile1 (not . isSpace))
        versionP = P.skipWhile isSpace
                 *> ((== "2") <$> P.takeWhile1 (not . isSpace))

data Shell = Windows | InWSL deriving (Eq, Show)

 -- | exit failure
newtype ShellException
    = ShellException Text
  deriving (Show)
  deriving anyclass (Exception)

-- | Somethings wrong with controller
newtype ControllerException
    = ControllerException String
  deriving (Show)
  deriving anyclass (Exception)


runDevevIO :: Interpreter Devenv (Logger : IOE : Errors [DriverException, UserError, Panic])
runDevevIO = interpret $ \case
    GetStatus -> getControllerStatus >>= \case
        WSLNotInstalledOrNeedsUpdate -> throwError $ DriverException $
            ControllerException $ "WSL is not installed or needs an update.\r\n" <>
                "Please run 'wsl --install --web-download --no-distribution' or " <>
                "download the latest version from 'https://github.com/microsoft/WSL/releases'.\r\n" <>
                "This will require admin privileges and also restarting your computer."
        ControllerNotInstalled       -> return DevenvNotInstalled
        ControllerInstalled status   -> return $ DevenvInstalled status

    InitCtrl ->
        liftIO (getXdgDirectoryList XdgDataDirs >>= flip findFile "devenv\\controller.tar.gz") >>= \case
            Nothing ->
                throwError $ Panic $ UnrecoverableError "Couldn't find rootfs for devenv controller"
            Just rootfs -> do
                ctrlDir <- liftIO $ getOrCreateXdgDir XdgState _DEVENV_CONTROLLER_DIR
                let ctrlFile = toNTPath $
                        ctrlDir `joinPaths` fromPathSegments ["controller.tar.gz"]
                liftIO $ copyFile rootfs (toString ctrlFile)

                void $ rethrowAll $ readProcess_ $
                    wsl'exeCmd [ "--import"
                               , _DEVENV_CONTROLLER
                               , toNTPath ctrlDir
                               , ctrlFile
                               ]



    Start alreadyRunning -> runDevevIO $ rethrowAll $ mainLoop alreadyRunning

    ListInstances -> do
        instances <- listWSLInstances
        return
            [ DevenvInstance devenvName _wslStatus
            | WSLInstance{..} <- instances
            , devenvName <- maybeToList $
                rightToMaybe . parse =<< Text.stripPrefix _DEVENV_INSTANCE_PREFIX _wslName
            ]

    RunCtrlNixCmd printLog cmd -> rethrowAll $ do
        let printWSL Win _   = pass
            printWSL WSL txt = printOutIO txt
            rp | printLog  = readProcessWith printWSL printWSL
               | otherwise = readProcess
        result <- rp $ controllerCmd cmd

        case result of
            Left err ->
                case Text.splitOn "error: " err of
                    [_, e] | not (Text.null e) -> return $ Left e
                    _                          -> throwError $ ShellException err
            Right out -> return $ Right out

    RunInstanceCmd printLog inst cmd -> rethrowAll $ do
        let printWSL Win _   = pass
            printWSL WSL txt = printOutIO txt
            rp | printLog  = readProcessWith printWSL printWSL
               | otherwise = readProcess
        result <- rp $ wslCmd (getInstanceNameWithPrefix inst) cmd

        case result of
            Left err  -> return $ Left err
            Right out -> return $ Right out

    InstallRootfs name rootfsPath -> do

        instanceDir <- liftIO $
            getOrCreateXdgDir XdgState $
                "instances" `joinPaths` mkNTPath (getInstanceName name)

        putTextLn $ "Installing " <> show (getInstanceName name) <> " from " <> toText rootfsPath

        void $ rethrowAll $ readProcess_ $
            wsl'exeCmd [ "--import"
                       , getInstanceNameWithPrefix name
                       , toNTPath instanceDir
                       , toText rootfsPath
                       ]

    UninstallInstance name -> do
        putTextLn $ "Uninstalling " <> getInstanceName name
        void $ rethrowAll $ readProcess_ $
            wsl'exeCmd [ "--unregister" , getInstanceNameWithPrefix name ]

    RunInInstance i showTerm args -> do
        liftIO $ do
            consoleWindow <- getConsoleWindow
            case consoleWindow of
                Just hwnd | not showTerm -> failIfFalse_ "hide console window" $ showWindow hwnd sW_HIDE
                _ -> pass
        let wrapIO | showTerm  = Proc.setStdin (Proc.useHandleOpen stdin)
                               . Proc.setStdout (Proc.useHandleOpen stdout)
                               . Proc.setStderr (Proc.useHandleOpen stderr)
                   | otherwise = Proc.setStdin Proc.nullStream
                               . Proc.setStdout Proc.nullStream
                               . Proc.setStderr Proc.nullStream

        Proc.runProcess_
            $ wrapIO
            $ wsl'exeCmd
            $ [ "-d", getInstanceNameWithPrefix $ _name i
              , "--shell-type", "login"
              ] <> args

    UpdateShortcuts name swSharePath -> runDevevIO $ rethrowAll $ Windows.updateShortcuts name swSharePath

    GetDevenvModule -> return "devenv-wsl"
    GetPath dirType s -> toString . toNTPath . (`joinPaths` s)
        <$> case dirType of
            DirCtrl   -> return $ getWSLInstanceDir _DEVENV_CONTROLLER
            DirState  -> getOrCreateXdgDir XdgState ""
            DirConfig -> getOrCreateXdgDir XdgConfig ""

mainLoop :: [IOE, Error ShellException, Error Panic, Logger, Devenv] :>> es => Bool -> Eff es ()
mainLoop _alreadyRunning = do
    pulseaudioLogFile <- getPath DirState $ fromPathSegments [ "pulseaudio.log" ]
    let pulseaudioLogLevel = case logLevel of
            LevelError -> 0 :: Int
            LevelWarn  -> 1
            LevelDebug -> 4
            _          -> 3
    vcxsrvLogFile <- getPath DirState $ fromPathSegments [ "vcxsrv.log" ]
    let vcxsrvLogLevel = case logLevel of
            LevelError -> 0 :: Int
            LevelWarn  -> 1
            LevelDebug -> 3
            _          -> 2

    cancelChan <- atomically newTChan


    logInfo "Waking up WSL..."
    void $ runProcessSilent $ controllerCmd "true"
    wslVmId <- fmap (logTraceId "wslVmId")
             . maybe (throwError $ Panic $ UnrecoverableError "Can't retrieve WSL's VM ID...") return
           =<< liftIO findWslVmId

    let subprog name cmd args = do
            myCancelChan <- atomically $ dupTChan cancelChan
            void $ runProcessSilent
                 $ shellProc "taskkill.exe" [ "/F", "/IM" , name ]
            let doLog lvl _ txt = runTChanLoggingT $ logWithoutLoc name lvl txt
            fix $ \loop -> do
                runProcessWith (doLog LevelInfo) (doLog LevelWarn) (shellProc cmd args)
                whenNothingM_ (atomically $ tryReadTChan myCancelChan) $ do
                    logIO LevelWarn $ name <> " exited unexpectedly. Restarting..."
                    threadDelay 1_000_000
                    loop

    void $ liftIO $ async $  subprog "devenv_pulseaudio.exe" "C:\\Program Files (x86)\\PulseAudio\\bin\\devenv_pulseaudio.exe"
            [ "--log-target=file:" <> toText pulseaudioLogFile
            , "--log-time"
            , "--log-level=" <> show pulseaudioLogLevel
            , "--disallow-exit"
            , "--disallow-module-loading"
            -- , "--system"
            , "--exit-idle-time=-1"
            ]
    void $ liftIO $ async $ subprog "devenv_vcxsrv.exe" "C:\\Program Files\\VcXsrv\\devenv_vcxsrv.exe"
            [ "-ac"             -- disable access control
            , "-noreset"        -- dont restart after last client exits
            , "-wgl"            -- native opengl
            , "-compositewm"    -- previews for windows
            , "-notrayicon"
            , "-dpi", "auto"
            , "-multiwindow"    -- seamless mode
            , "-clipboard", "-noprimary"
            , "-logfile", toText vcxsrvLogFile
            , "-logverbose", show vcxsrvLogLevel
            , "-vmid", "{" <> toText wslVmId <> "}"
            , "-vsockport", "6000"
            ]
            -- https://sourceforge.net/p/vcxsrv/discussion/986201/thread/1ab552d067/

    void $ liftIO $ async $ do
            myCancelChan <- atomically $ dupTChan cancelChan
            void $ readProcess $ controllerCmd "pkill nix-daemon || true"
            let doLog lvl _ txt = runTChanLoggingT $ logWithoutLoc "devenv_controller" lvl txt
            fix $ \loop -> do
                runProcessWith (doLog LevelInfo) (doLog LevelWarn)
                    $ Proc.setStdin Proc.closed
                    $ controllerCmd "/bin/devenv-serve"
                whenNothingM_ (atomically $ tryReadTChan myCancelChan) $ do
                    doLog LevelWarn Win ("Devenv controller exited unexpectedly (please see logs). Restarting..." :: Text)
                    loop

    runWinLoop $ do
        putTextLn "Stopping devenv..."
        atomically $ writeTChan cancelChan ()
        void $ runProcessSilent $ controllerCmd "pkill nix-daemon || true"
        -- void $ runProcessSilent $ shellProc "taskkill.exe" [ "/F", "/IM", "devenv_pulseaudio.exe" ]
        void $ runProcessSilent $ shellProc "taskkill.exe" [ "/F", "/IM", "devenv_vcxsrv.exe" ]


runProcessSilent :: MonadIO m => ProcessConfig stdin stdout stderr -> m ExitCode
runProcessSilent = Proc.runProcess
           . Proc.setStdout Proc.nullStream
           . Proc.setStderr Proc.nullStream

updateShortcuts :: [IOE, Error ShellException, Error Panic, Logger, Devenv] :>> es
                => InstanceName
                -> FilePath
                -> Eff es ()
updateShortcuts name swSharePath = do
    icosFolder <- toString . toNTPath
              <$> getOrCreateXdgDir XdgState (fromPathSegments [ "icos", getInstanceName name ])

    -- cleanup start menu entries
    mapM_ (removeFile . (\f -> icosFolder <> "\\" <> f)) =<< listDirectory icosFolder

    desktopEntries <-
        let parseDesktopEntryAndIco app = do
                de <- parseDesktopEntry . decodeUtf8
                   <$> readFileBS (swSharePath <> "\\wsl\\applications\\" <> toString app <> ".desktop")

                case de of
                    Left err -> throwError $ Panic $ UnrecoverableError $
                        "Could not parse desktop entry for " <> app <> ":\n" <> err
                    Right (DesktopEntry {deName,deTerminal,deExec}) -> do
                        let iconInCtrl = swSharePath <> "\\wsl\\icos\\" <> toString app <> ".ico"
                            iconInWin  = icosFolder <> "\\" <> toString deName <> ".ico"
                        deIcon <- doesFileExist iconInCtrl >>= \case 
                            True -> do
                                copyFile iconInCtrl iconInWin
                                return $ Just iconInWin
                            False -> do
                                log LevelWarn $ "Could not find .ico for " <> app
                                return Nothing
                        return $ DesktopEntry {..}


         in mapM parseDesktopEntryAndIco
         .  mapMaybe (Text.stripSuffix ".desktop" . toText)
        =<< listDirectory (swSharePath <> "\\wsl\\applications")

    startMenuFolder <- liftIO $ do
        startMenu <- getFolderPath cSIDL_PROGRAMS
        let instanceFolder = startMenu `joinPaths` fromPathSegments [ _DEVENV_APP_NAME, getInstanceName name ]
        createDirectoryIfMissing True (toString $ toNTPath instanceFolder)
        return $ toString $ toNTPath instanceFolder

    -- cleanup start menu entries
    mapM_ (removeFile . (\f -> startMenuFolder <> "\\" <> f)) =<< listDirectory startMenuFolder

    -- create shortcuts
    -- currentDir <- getCurrentDirectory
    homeDir <- getHomeDirectory
    either (throwError . Panic . UnrecoverableError . show) return
        =<< liftIO (runExceptT $ do
                ExceptT initialize
                forM_ desktopEntries $ \(DesktopEntry {..}) -> do
                    let lnk = Shortcut
                            { targetPath = _DEVENV_APP_NAME <> ".exe"
                            , arguments = intercalate " " $
                                [ "run" ] <>
                                [ "--no-terminal" | not deTerminal ] <>
                                [ toString $ getInstanceName name
                                , "--"
                                , toString deExec
                                ]
                            , workingDirectory = homeDir
                            , showCmd = if deTerminal then ShowNormal else ShowMinimized
                            , description = ""
                            , iconLocation = (fromMaybe "" deIcon, 0)
                            , hotkey = 0
                            }
                        lnkPath = startMenuFolder <> "\\" <> toString deName <> ".lnk"
                    ExceptT $ writeShortcut lnk lnkPath
                liftIO uninitialize
            )
    liftIO refreshIconCache



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

-- https://tarma.com/support/im9/using/symbols/functions/csidls.htm
cSIDL_PROGRAMS :: CSIDL
cSIDL_PROGRAMS = 2

getFolderPath :: CSIDL -> IO (Path Abs)
getFolderPath csidl = fromString <$> sHGetFolderPath nullPtr csidl nullPtr 0

getOrCreateXdgDir :: MonadIO m => XdgDirectory -> Path Rel -> m (Path Abs)
getOrCreateXdgDir xdg dir = liftIO $ do
    path <- fromString <$> getXdgDirectory xdg _DEVENV_APP_NAME
    let innerPath = path `joinPaths` dir
    createDirectoryIfMissing True (toString $ toNTPath innerPath)
    return innerPath

instance IsString (Path x) where
    fromString = mkNTPath . toText

rethrowAll
    :: forall catchAllErr errs rest a.
     ( catchAllErr ~ Error DriverException
     , catchAllErr :> rest
     , errs ~ Errors
         [ ParseException
         , ShellException
         -- , WinException
         -- , WSLException
         -- , DecodeException
         ] )
    => Eff (errs ++ rest) a
    -> Eff rest a
rethrowAll = pluckEffects @catchAllErr @errs

controllerCmd :: Text -> ProcessConfig () () ()
controllerCmd = wslCmd _DEVENV_CONTROLLER

wslCmd :: Text -> Text -> ProcessConfig () () ()
wslCmd name cmd = wsl'exeCmd $
    [ "-d", name
    , "--shell-type", "login"
    , "--user", "root"
    -- , "--cd", "/root"
    , "bash", "-ic"
    ]
    <> (["-x" | logLevel <= LevelDebug ]) <>
    [ show $ "printf " <> _MAGIC_UTF8_SEQ <> " | tee /dev/stderr ; " <> cmd
    ]

wsl'exeCmd :: [Text] -> ProcessConfig () () ()
wsl'exeCmd = shellCmd "wsl.exe"

shellCmd :: Text -> [Text] -> ProcessConfig () () ()
shellCmd cmd args = logTraceShowId "shellCmd" $
    Proc.shell $ toString $ unwords $ cmd : args

shellProc :: Text -> [Text] -> ProcessConfig () () ()
shellProc cmd args = logTraceShowId "shellProc" $
    Proc.proc (toString cmd) (map toString args)

listWSLInstances :: [Error DriverException, IOE] :>> es => Eff es [WSLInstance]
listWSLInstances = rethrowAll $ parseProcess_ @[WSLInstance] $ wsl'exeCmd [ "-l", "-v" ]

data WinDevenvStatus
    = WSLNotInstalledOrNeedsUpdate
    | ControllerNotInstalled
    | ControllerInstalled InstanceStatus


runProcessWith
    :: MonadUnliftIO m
    => (OutputType -> Text -> IO ())
    -> (OutputType -> Text -> IO ())
    -> ProcessConfig () () ()
    -> m ()
runProcessWith outLogger errLogger proc = do
    let proc' = proc
              & Proc.setStderr Proc.createPipe
              & Proc.setStdout Proc.createPipe
    Proc.withProcessTerm proc' $ \p -> do
        let send logger h = loop Win where
                loop prevType = do
                    bs <- BS.hGetSome h defaultChunkSize
                    let txt = decodeUtf16And8 $ toLazy bs
                        fixEOL | prevType == WSL = Text.replace "\n" "\r\n"
                               | otherwise = id
                    unless (BS.null bs) $
                        case map fixEOL $ Text.splitOn _MAGIC_UTF8_SEQ txt of
                            [win, wsl] -> do
                                void $ logger Win win
                                void $ logger WSL wsl
                                loop WSL
                            _          -> do
                                void $ logger prevType txt
                                loop prevType
        void $ liftIO $ async $ send outLogger (Proc.getStdout p)
        void $ liftIO $ async $ send errLogger (Proc.getStderr p)
        void $ Proc.waitExitCode p

data OutputType = Win | WSL deriving (Eq)

readProcessWith
    :: MonadUnliftIO m
    => (OutputType -> Text -> IO ())
    -> (OutputType -> Text -> IO ())
    -> ProcessConfig () () ()
    -> m (Either Text Text)
readProcessWith outLogger errLogger proc = do
    let proc' = Proc.setStdout (loggingTextOutput outLogger)
              $ Proc.setStderr (loggingTextOutput errLogger)
              $ logTraceShowId "readProcess" proc
        collect p = atomically $
                         (,,) <$> Proc.waitExitCodeSTM p
                              <*> Proc.getStdout p
                              <*> Proc.getStderr p
        splitWinWSL txt = case Text.splitOn _MAGIC_UTF8_SEQ txt of
            [_, wsl] | not (Text.null wsl) -> wsl
            _                              -> txt
    (exitCode, out, err) <- logTraceShowId "readProcess result"
                        <$> Proc.withProcessTerm proc' collect
    case exitCode of
        ExitSuccess   -> return $ Right $ logTraceId "splitWinWSL" $ splitWinWSL $ toStrict out
        ExitFailure _ -> return $ Left  $ logTraceId "splitWinWSL" $ splitWinWSL $ toStrict err

loggingTextOutput :: (OutputType -> Text -> IO ()) -> Proc.StreamSpec 'Proc.STOutput (STM LText)
loggingTextOutput logger = Proc.mkPipeStreamSpec loggingTextOutputFromHandle where
    loggingTextOutputFromHandle :: ProcessConfig () () () -> Handle -> IO (STM LText, IO ())
    loggingTextOutputFromHandle pc h = Utf8.withHandle h $ do
        mvar <- newEmptyTMVarIO

        void $ async $ do
            let loop prevType front = do
                    bs <- BS.hGetSome h defaultChunkSize
                    let txt = decodeUtf16And8 $ toLazy bs
                        fixEOL | prevType == WSL = Text.replace "\n" "\r\n"
                               | otherwise = id
                    if BS.null bs
                        then atomically $ putTMVar mvar $ Right $ LText.fromChunks $ front []
                        else case map fixEOL $ Text.splitOn _MAGIC_UTF8_SEQ txt of
                            [win, wsl] -> do
                                logger Win win
                                logger WSL wsl
                                loop WSL $ front . (wsl:) . (win:)
                            _          -> do
                                logger prevType txt
                                loop prevType $ front . (txt:)
            loop Win id `catch` \e -> do
                atomically $ void $ tryPutTMVar mvar $ Left $ Proc.ByteStringOutputException e pc
                throwIO e

        return (readTMVar mvar >>= either throwSTM return, hClose h)

-- catchIf
--     :: (MonadUnliftIO m, Exception e)
--     => (e -> Bool) -> m a -> (e -> m a) -> m a
-- catchIf f a b = catch a (\e -> if f e then b e else throwIO e)

readProcess :: MonadUnliftIO m => ProcessConfig () () () -> m (Either Text Text)
readProcess = readProcessWith (\_ _ -> pass) (\_ _ -> pass)

decodeUtf16And8 :: BL.ByteString -> Text
decodeUtf16And8 = decodeUtf8 . BL.filter (/= 0)

readProcess_ :: [IOE, Error ShellException] :>> es => ProcessConfig () () () -> Eff es Text
readProcess_ = either (throwError . ShellException) return <=< readProcess

parseProcess :: (MonadUnliftIO m, Parseable a) => ProcessConfig () () () -> m (Either Text a)
parseProcess = fmap (parse =<<) . readProcess

parseProcess_
    :: forall a es. ( [Error ParseException, IOE] :>> es
                    , Parseable a
                    )
    => ProcessConfig () () ()
    -> Eff es a
parseProcess_ = either (throwError . ParseException) return <=< parseProcess


getControllerStatus :: [Error Panic, IOE] :>> es => Eff es WinDevenvStatus
getControllerStatus =
    readProcess "wsl.exe --version" >>= \case
        -- WSL is not activated or needs an update (--version is only available
        -- in later versions)
        Left _ -> return WSLNotInstalledOrNeedsUpdate
        Right _ -> readProcess "wsl.exe -l -v" >>= \case
                Right out -> do
                    instances <- either (throwError . Panic . ParseException) return $
                        parse @[WSLInstance] out
                    case find ((== _DEVENV_CONTROLLER) . _wslName) instances of
                        Nothing     -> return ControllerNotInstalled
                        Just status -> return $ ControllerInstalled (_wslStatus status)
                -- WSL outputs an error message to stdout if no distribution is
                -- installed... We assume that if there is another error, there
                -- will be an error message
                Left err | Text.null err -> return ControllerNotInstalled
                Left err -> throwError $ Panic $ UnrecoverableError $ logTraceId "wsl -l -v" err


getWSLInstanceDir :: Text -> Path Abs
getWSLInstanceDir instanceName = fromPathSegments ["\\\\wsl$", instanceName]

_MAGIC_UTF8_SEQ :: IsString s => s
_MAGIC_UTF8_SEQ = "this_is_for_recognizing_utf8"

-- splitMagicUTF8Seq txt = case Text.splitOn _MAGIC_UTF8_SEQ txt of
--     [win, wsl] -> (win, wsl)
--     _ | _MAGIC_UTF8_SEQ `Text.isInfixOf` txt -> ("", txt)


-- https://otter-o.hatenadiary.org/entry/20090217/1234861028
runWinLoop :: MonadIO m => IO () -> m ()
runWinLoop cleanup = liftIO $ do
    withConsoleCtrlHandler (\_ -> cleanup >> exitSuccess >> pure True) $ do
        void $ createMessageWindow msgLoop
        allocaMessage messagePump

    where
        createMessageWindow wndProc = do
            let clsName = mkClassName _DEVENV_APP_NAME
            hinst <- getModuleHandle Nothing
            whenNothingM_ (registerClass (0, hinst, Nothing, Nothing, Nothing, Nothing, clsName)) $
                error "Couldn't register window class"
            createWindow
                clsName
                _DEVENV_APP_NAME
                0
                Nothing
                Nothing
                Nothing
                Nothing
                (Just nullPtr)
                Nothing
                hinst
                wndProc
        msgLoop hwnd msg wp lp
            | msg == wM_CLOSE   = cleanup >> exitSuccess >> return 0
            | otherwise         = liftIO $ defWindowProc (Just hwnd) msg wp lp
        messagePump msg = void $ infinitely $ do
            whenM (getMessage msg Nothing) $ do
                void $ translateMessage msg
                void $ dispatchMessage msg

findWslVmId :: IO (Maybe String)
findWslVmId = do
    let vmsKey = regOpenKeyEx hKEY_LOCAL_MACHINE "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\HostComputeService\\VolatileStore\\ComputeSystem" kEY_READ
    withRegKey vmsKey $ \hkey -> do
        vmKeys <- regEnumKeys hkey

        let filterWslVms vmid = do
                subkeys <- withRegKey (regOpenKeyEx hkey vmid kEY_READ) regEnumKeyVals

                return $
                    if any isWslVm subkeys
                        then Just vmid
                        else Nothing

        asum <$> mapM filterWslVms vmKeys

    where
        isWslVm ("ComputeSystemType", "2", _) = True
        isWslVm _                             = False

withRegKey :: IO HKEY -> (HKEY -> IO c) -> IO c
withRegKey aquire = bracket aquire regCloseKey


#ifdef mingw32_HOST_OS
foreign import ccall "shlobj_core.h SHChangeNotify"
    c_SHChangeNotify :: LONG -> UINT32 -> LPVOID -> LPVOID -> IO ()

refreshIconCache :: IO ()
refreshIconCache = c_SHChangeNotify 0x8000000 0x1000 nullPtr nullPtr

foreign import ccall "windows.h FreeConsole"
    c_FreeConsole :: IO BOOL

freeConsole :: IO ()
freeConsole = failIfFalse_ "FreeConsole" c_FreeConsole

foreign import ccall "windows.h GetConsoleWindow"
    c_GetConsoleWindow :: IO HWND

getConsoleWindow :: IO (Maybe HWND)
getConsoleWindow = do
    hwnd <- c_GetConsoleWindow
    return $ if hwnd /= nullPtr
        then Just hwnd
        else Nothing

#else

-- fake functions for HLS under linux

noop :: a
noop = error "Not implemented on linux"

type DWORD = Word32
type WindowStyle = DWORD
type LONG = Int32
type LPMSG = Ptr ()
type HWND = Ptr ()
type HMENU = Ptr ()
type HANDLE = Ptr ()
type HINSTANCE = Ptr ()
type LPTSTR = Ptr CWchar
type WindowClosure = HWND -> DWORD -> Word -> CIntPtr -> IO CIntPtr
type REGSAM = Word32
type HKEY = ForeignPtr ()
type CSIDL = CInt

wM_CLOSE :: DWORD
wM_CLOSE = noop
-- wM_DESTROY :: DWORD
-- wM_DESTROY = noop

failIfFalse_ :: String -> IO Bool -> IO ()
failIfFalse_ = noop

withConsoleCtrlHandler :: (DWORD -> IO Bool) -> IO a -> IO a
withConsoleCtrlHandler = noop

getMessage :: LPMSG -> Maybe HWND -> IO Bool
getMessage = noop
allocaMessage :: (LPMSG -> IO a) -> IO a
allocaMessage = noop
translateMessage :: LPMSG -> IO Bool
translateMessage = noop
dispatchMessage :: LPMSG -> IO LONG
dispatchMessage = noop
defWindowProc :: Maybe HWND -> DWORD -> Word -> CIntPtr -> IO CIntPtr
defWindowProc = noop
-- postQuitMessage :: Int -> IO ()
-- postQuitMessage = noop

mkClassName :: String -> a
mkClassName = noop
registerClass :: (Word32, HINSTANCE, Maybe HANDLE, Maybe HANDLE, Maybe HANDLE, Maybe LPTSTR, LPTSTR) -> IO (Maybe Word16)
registerClass = noop
createWindow
  :: LPTSTR -> String -> WindowStyle ->
     Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int ->
     Maybe HWND -> Maybe HMENU -> HINSTANCE -> WindowClosure ->
     IO HWND
createWindow = noop

type ShowWindowControl = DWORD
sW_HIDE :: ShowWindowControl
sW_HIDE = noop
showWindow :: HWND -> ShowWindowControl -> IO Bool
showWindow = noop

getModuleHandle :: Maybe String -> IO HINSTANCE
getModuleHandle = noop

hKEY_LOCAL_MACHINE :: HKEY
hKEY_LOCAL_MACHINE = noop
kEY_READ :: REGSAM
kEY_READ = noop
regOpenKeyEx :: HKEY -> String -> REGSAM -> IO HKEY
regOpenKeyEx = noop
regCloseKey :: HKEY -> IO ()
regCloseKey = noop
regEnumKeys :: HKEY -> IO [String]
regEnumKeys = noop
regEnumKeyVals :: HKEY -> IO [(String,String,DWORD)]
regEnumKeyVals = noop

type SHGetFolderPathFlags = DWORD
sHGetFolderPath :: HWND -> CSIDL -> HANDLE -> SHGetFolderPathFlags -> IO String
sHGetFolderPath = noop

-- win32-shortcut

data ShowCmd = ShowNormal | ShowMaximized | ShowMinimized deriving (Show)
data Shortcut = Shortcut
    { targetPath       :: FilePath
    , arguments        :: String
    , workingDirectory :: FilePath
    , showCmd          :: ShowCmd
    , description      :: String
    , iconLocation     :: (FilePath, Int)
    , hotkey           :: DWORD
    }
  deriving (Show)

type ShortcutError = Void
initialize :: IO (Either ShortcutError ())
initialize = noop
uninitialize :: IO ()
uninitialize = noop
writeShortcut :: Shortcut -> FilePath -> IO (Either ShortcutError ())
writeShortcut = noop
refreshIconCache :: IO ()
refreshIconCache = noop

-- freeConsole :: IO ()
-- freeConsole = noop
getConsoleWindow :: IO (Maybe HWND)
getConsoleWindow = noop
#endif
