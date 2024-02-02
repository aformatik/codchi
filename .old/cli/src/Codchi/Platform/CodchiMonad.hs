{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Codchi.Platform.CodchiMonad where

import Byline (BylineT, runBylineT)
import Codchi.Config
import Codchi.Error
import Codchi.Types
import qualified Control.Exception.Annotated.UnliftIO as Ann
import Data.Char (toLower)
import RIO hiding (exitFailure, hSetBuffering, show)
import RIO.Process (HasProcessContext (..), ProcessContext, mkDefaultProcessContext)

data StreamOutput = StreamStd | StreamIgnore
    deriving (Show, Eq)

data DriverMeta = DriverMeta
    { moduleName :: Text
    -- ^ Name in codchi's NixOS modules (`codchi.internal.<DRIVER>.enable = true`)
    }
    deriving (Show, Eq, Generic)

class
    ( Functor m
    , Applicative m
    , Monad m
    , MonadReader Codchi m
    , MonadIO m
    , MonadUnliftIO m
    , MonadThrow m
    , MonadLogger Codchi m
    ) =>
    MonadCodchi m
    where
    driverMeta :: m DriverMeta

    -- | Get status of the Codchi app (currently: not installed / stopped /
    -- started)
    getStatus :: m CodchiStatus

    -- | Get status of code machines (currently: not installed / stopped /
    -- started / orphaned)
    listMachines :: m [CodeMachine]

    -- | Register the controller rootfs archive with the driver. The rootfs is
    -- either included in the installation package (MSIX / RPM / ...) or built
    -- via Nix
    controllerInit :: m ()

    -- | Start the controller container, do shared mounts (/nix, ...), launch
    -- nix-daemon, start accessory programs (X-Server, ...), start GUI.
    -- This should be a no-op if already running.
    controllerStart :: m ()

    -- | Register rootfs archive from disk with the driver
    -- This should be a no-op if already installed
    driverInstallInstance :: CodchiName -> FilePath -> m ()

    -- | Unregister instance from driver and delete all state on disk which is
    -- associated with the instance (shared mounts, shortcuts).
    driverUninstallInstance :: CodchiName -> MachineStatus -> m ()

    -- | Run root command in instance
    runInInstance :: InstanceConfig -> Bool -> [Text] -> m ()

    -- | Synchronize shortcuts on host with instance shortcut. FilePath is path
    -- to $currentSystem/sw/share.
    updateShortcuts :: CodchiName -> FilePath -> m ()

    -- | Run command in controller instance and return error if stderr contains
    -- 'error: ' (error thrown by Nix).
    runCtrlNixCmd :: StreamOutput -> Text -> m (Either String Text)

    -- | Run command as default user in instance
    runInstanceCmd :: StreamOutput -> CodchiName -> Text -> m (Either Text Text)

    -- | Get path on host for given directory type. The path of the directory
    -- type should be created if it doesn't exist.
    getDriverPath :: DirectoryType -> Path Rel -> m FilePath

    -- | Get path in controller for path on host OS. Currently only used for
    -- local NixOS configs with `codchi add-module`.
    getControllerPath :: FilePath -> m (Either String (Path Abs))

{-# DEPRECATED runInInstance "Should be migrated to runInstanceCommand" #-}

data Codchi = Codchi
    { logFunc :: !LogFunc
    , logLevel :: !LogLevel
    , processContext :: !ProcessContext
    }

instance HasLogFunc Codchi where
    logFuncL = lens (.logFunc) (\env logFunc -> env{logFunc})

instance HasProcessContext Codchi where
    processContextL = lens (.processContext) (\env processContext -> env{processContext})

runCodchi :: RIO Codchi a -> LogOptions -> IO a
runCodchi app opts = do
    processContext <- mkDefaultProcessContext
    logLevel <- lookupLogLevel
    withLogFunc opts $ \logFunc ->
        let codchi = Codchi{logFunc, processContext, logLevel}
         in runRIO codchi $
                app
                    `Ann.catch` (\(e :: CodchiError) -> logExit (show e))
                    `catchAny` (\other -> logError (fromString $ displayException other) >> throwM other)

--------------------------------------
-- Additional MonadCodchi functions --
--------------------------------------

findCodeMachine :: MonadCodchi m => CodchiName -> m (Maybe CodeMachine)
findCodeMachine name = find (\cm -> name == cm.name) <$> listMachines

findCodeMachine_ :: MonadCodchi m => CodchiName -> m CodeMachine
findCodeMachine_ name =
    whenNothingM (findCodeMachine name) $
        Ann.throw . InternalPanic $
            "Could not find machine " <> toString name.text

runCtrlNixCmd_ :: MonadCodchi m => StreamOutput -> Text -> m Text
runCtrlNixCmd_ stream cmd =
    either (Ann.throw . InternalNixError) return
        =<< runCtrlNixCmd stream cmd

-----------------------
-- Logging / Tracing --
-----------------------

type MonadLogger e m = (HasLogFunc e, MonadReader e m, MonadIO m)

lookupLogLevel :: MonadIO m => m LogLevel
lookupLogLevel = do
    lvlStr <- fromMaybe "" <$> lookupEnv "LOG"
    return $ case map toLower lvlStr of
        "error" -> LevelError
        "warn" -> LevelWarn
        "info" -> LevelInfo
        "debug" -> LevelDebug
        _ -> LevelInfo

initOptions :: MonadIO m => (Bool -> m LogOptions) -> m LogOptions
initOptions mkOpts = do
    logLevel <- lookupLogLevel
    let isVerbose = logLevel == LevelDebug
        mods = setLogMinLevel logLevel

    mods <$> mkOpts isVerbose

withStderrLogging :: (MonadIO m) => (LogOptions -> m a) -> m a
withStderrLogging act = initOptions (logOptionsHandle stderr) >>= act

withFileLogging :: (MonadCodchi m, m ~ RIO Codchi) => Text -> (LogOptions -> m a) -> m a
withFileLogging name act = do
    logFile <-
        liftIO . withStderrLogging . runCodchi $
            getDriverPath DirState (fromList [name <> ".log"])
    bracket (openFile logFile AppendMode) hClose $
        \h -> do
            hSetBuffering h LineBuffering
            opts <- initOptions (logOptionsHandle h)
            act opts

logExit :: HasLogFunc e => Utf8Builder -> RIO e b
logExit msg = logError msg >> exitFailure

logTrace :: (Display d, MonadLogger e m) => LogSource -> d -> a -> m a
logTrace loc msg x = x <$ logDebugS loc (display msg)

logTraceId :: (Show s, MonadLogger e m) => LogSource -> s -> m s
logTraceId loc msg = logTrace loc (show @Text msg) msg

withConsole :: MonadIO m => BylineT IO a -> m (Maybe a)
withConsole = liftIO . runBylineT

withConsole_ :: MonadIO m => BylineT IO a -> m ()
withConsole_ = void . withConsole
