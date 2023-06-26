{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Windows
    ( runDevevIO
    ) where


#ifdef mingw32_HOST_OS
import           Graphics.Win32
import           System.Win32.Console.CtrlHandler
import           System.Win32.DLL                 (getModuleHandle)
import           System.Win32.Registry
#else
import           Foreign                          (ForeignPtr, Ptr, nullPtr)
import           Foreign.C                        (CIntPtr, CWchar)
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

import           Control.Concurrent.Async
import           Control.Concurrent.STM           (dupTChan, newTChan, tryReadTChan, writeTChan)
import           Control.Monad.Logger             (LogLevel (..), logWithoutLoc)
import           Data.ByteString.Builder.Extra    (defaultChunkSize)
import           System.Directory
import qualified System.Process.Typed             as Proc
import           System.Process.Typed             (ExitCode (..), ProcessConfig)
import           UnliftIO                         (bracket, catch, hClose, throwIO)
import Control.Concurrent (threadDelay)

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
            ControllerException "WSL is not installed or needs an update"
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

    RunInstance i -> do
        Proc.runProcess_ $ Proc.setStdin (Proc.useHandleOpen stdin)
                         $ Proc.setStdout (Proc.useHandleOpen stdout)
                         $ Proc.setStderr (Proc.useHandleOpen stderr)
                         $ wsl'exeCmd [ "-d", getInstanceNameWithPrefix $ _name i ]

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
                    $ controllerCmd "/bin/serve"
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
wslCmd name cmd = wsl'exeCmd
    [ "-d", name
    , "--shell-type", "login"
    , "--user", "root"
    -- , "--cd", "/root"
    , "bash", "-ic"
    , show $ "printf " <> _MAGIC_UTF8_SEQ <> " | tee /dev/stderr ; " <> cmd
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
    loggingTextOutputFromHandle pc h = do
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
                -- WSL outputs an error message to stdout if no distribution is
                -- installed... We assume that if there is another error, there
                -- will be an error message
                Left err | Text.null err -> return ControllerNotInstalled
                Left err -> throwError $ Panic $ UnrecoverableError $ logTraceId "wsl -l -v"  err
                Right out -> do
                    instances <- either (throwError . Panic . ParseException) return $
                        parse @[WSLInstance] out
                    case find ((== _DEVENV_CONTROLLER) . _wslName) instances of
                        Nothing     -> return ControllerNotInstalled
                        Just status -> return $ ControllerInstalled (_wslStatus status)


getWSLInstanceDir :: Text -> Path Abs
getWSLInstanceDir instanceName = fromPathSegments ["\\\\wsl$", instanceName]

_MAGIC_UTF8_SEQ :: IsString s => s
_MAGIC_UTF8_SEQ = "this_is_for_recognizing_utf8"

-- splitMagicUTF8Seq txt = case Text.splitOn _MAGIC_UTF8_SEQ txt of
--     [win, wsl] -> (win, wsl)
--     _ | _MAGIC_UTF8_SEQ `Text.isInfixOf` txt -> ("", txt)


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


-- fake functions for HLS under linux
#ifndef mingw32_HOST_OS

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

wM_CLOSE :: DWORD
wM_CLOSE = error "Not implemented on linux"
-- wM_DESTROY :: DWORD
-- wM_DESTROY = error "Not implemented on linux"

withConsoleCtrlHandler :: (DWORD -> IO Bool) -> IO a -> IO a
withConsoleCtrlHandler = error "Not implemented on linux"

getMessage :: LPMSG -> Maybe HWND -> IO Bool
getMessage = error "Not implemented on linux"
allocaMessage :: (LPMSG -> IO a) -> IO a
allocaMessage = error "Not implemented on linux"
translateMessage :: LPMSG -> IO Bool
translateMessage = error "Not implemented on linux"
dispatchMessage :: LPMSG -> IO LONG
dispatchMessage = error "Not implemented on linux"
defWindowProc :: Maybe HWND -> DWORD -> Word -> CIntPtr -> IO CIntPtr
defWindowProc = error "Not implemented on linux"
-- postQuitMessage :: Int -> IO ()
-- postQuitMessage = error "Not implemented on linux"

mkClassName :: String -> a
mkClassName = error "Not implemented on linux"
registerClass :: (Word32, HINSTANCE, Maybe HANDLE, Maybe HANDLE, Maybe HANDLE, Maybe LPTSTR, LPTSTR) -> IO (Maybe Word16)
registerClass = error "Not implemented on linux"
createWindow
  :: LPTSTR -> String -> WindowStyle ->
     Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int ->
     Maybe HWND -> Maybe HMENU -> HINSTANCE -> WindowClosure ->
     IO HWND
createWindow = error "Not implemented on linux"
-- showWindow :: HWND -> DWORD -> IO Bool
-- showWindow = error "Not implemented on linux"

getModuleHandle :: Maybe String -> IO HINSTANCE
getModuleHandle = error "Not implemented on linux"

hKEY_LOCAL_MACHINE :: HKEY
hKEY_LOCAL_MACHINE = error "Not implemented on linux"
kEY_READ :: REGSAM
kEY_READ = error "Not implemented on linux"
regOpenKeyEx :: HKEY -> String -> REGSAM -> IO HKEY
regOpenKeyEx = error "Not implemented on linux"
regCloseKey :: HKEY -> IO ()
regCloseKey = error "Not implemented on linux"
regEnumKeys :: HKEY -> IO [String]
regEnumKeys = error "Not implemented on linux"
regEnumKeyVals :: HKEY -> IO [(String,String,DWORD)]
regEnumKeyVals = error "Not implemented on linux"

#endif
