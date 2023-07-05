{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
module Dsl where

import Cleff
import Cleff.Error                         (Error, runError)
import Control.Concurrent.STM              (TChan, newBroadcastTChanIO, writeTChan)
import Control.Monad.Logger                (LogLevel (..), LogLine, LoggingT, ToLogStr,
                                            logWithoutLoc, runLoggingT)
import Data.ByteString                     (hPut)
import Data.Char                           (toLower)
import System.Exit                         (ExitCode (..))
import System.IO.Unsafe
import Types
import Util                                (EffectInterpreter (..))

type Interpreter e m = forall es a. m :>> es => Eff (e : es) a -> Eff es a

data DirectoryType = DirState | DirConfig | DirCtrl

data CodchiL :: Effect where
    GetStatus :: CodchiL m CodchiStatus
    ListInstances :: CodchiL m [CodchiInstance]
    --
    InitCtrl :: CodchiL m ()
    Start :: Bool -> CodchiL m ()
    InstallRootfs :: InstanceName -> FilePath -> CodchiL m ()
    UninstallInstance :: InstanceName -> CodchiL m ()
    RunInInstance :: CodchiInstance -> Bool -> [Text] -> CodchiL m ()
    --
    -- | path to $currentSystem/sw/share
    UpdateShortcuts :: InstanceName -> FilePath -> CodchiL m ()
    --
    RunCtrlNixCmd :: Bool -> Text -> CodchiL m (Either Text Text)
    RunInstanceCmd :: Bool -> InstanceName -> Text -> CodchiL m (Either Text Text)
    GetPath :: DirectoryType -> Path Rel -> CodchiL m FilePath
    --
    -- | NixOS module in flake.nix for driver (e.g. driver-wsl)
    GetDriverModule :: CodchiL m Text
makeEffect ''CodchiL

data Logger :: Effect where
    Log :: LogLevel -> Text -> Logger m ()
    LogExit :: ExitCode -> Text -> Logger m a
    PrintOut :: Text -> Logger m ()
    PrintLnOut :: Text -> Logger m ()
    PrintErrExit :: ExitCode -> Text -> Logger m a
makeEffect ''Logger

instance (Exception e, Logger :> es ++ rest) => EffectInterpreter Logger (Error e : es) rest where
    interpretEffect = either (logExit (ExitFailure 1) . show) return <=< runError

interpretLogger :: Interpreter Logger '[IOE]
interpretLogger = interpret $ \case
    Log level msg         -> logIO level msg
    LogExit code msg      -> logExitIO code msg
    PrintOut msg          -> printOutIO msg
    PrintLnOut msg        -> printLnOutIO msg
    PrintErrExit code msg -> printErrExitIO code msg

runTChanLoggingT :: MonadIO m => LoggingT m a -> m a
runTChanLoggingT = (`runLoggingT` sink) where
    sink loc src lvl msg
        | lvl < logLevel = pass
        | otherwise = atomically $ writeTChan logChan $ Just (loc,src,lvl,msg)

logDebug, logInfo, logWarn, logError :: ('[Logger] :>> es) => Text -> Eff es ()
logDebug = log LevelDebug
logInfo = log LevelInfo
logWarn = log LevelWarn
logError = log LevelError

logIO :: MonadIO m => LogLevel -> Text -> m ()
logIO level msg         = runTChanLoggingT $ logWithoutLoc "" level msg
logExitIO :: MonadIO m => ExitCode -> Text -> m a
logExitIO code msg      = logIO LevelError msg >> exitWith code
printLnOutIO :: MonadIO m => Text -> m ()
printLnOutIO            = putTextLn
printOutIO :: MonadIO m => Text -> m ()
printOutIO              = putText
printErrExitIO :: MonadIO m => ExitCode -> Text -> m a
printErrExitIO code msg = liftIO $ hPut stderr (encodeUtf8 msg) >> exitWith code

logChan :: TChan (Maybe LogLine)
logChan = unsafePerformIO newBroadcastTChanIO
{-# NOINLINE logChan #-}

logLevel :: LogLevel
logLevel = unsafePerformIO $ do
    lvlStr <- fromMaybe "" <$> lookupEnv "LOG"
    return $ case map toLower lvlStr of
        "error" -> LevelError
        "warn"  -> LevelWarn
        "info"  -> LevelInfo
        "debug" -> LevelDebug
        _       -> LevelInfo
{-# NOINLINE logLevel #-}

logTrace :: ToLogStr s => String -> s -> a -> a
logTrace source msg x = unsafePerformIO $ do
    runTChanLoggingT $ logWithoutLoc (toText source) LevelDebug msg
    return x
{-# NOINLINE logTrace #-}

logTraceId :: ToLogStr s => String -> s -> s
logTraceId source x = logTrace source x x

logTraceShowId :: Show a => String -> a -> a
logTraceShowId source x = logTrace source (show @Text x) x
