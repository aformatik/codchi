module Codchi.Config.IO where

import Codchi.Config.Common
import Codchi.Config.V012
import Codchi.Platform.CodchiMonad
import Codchi.Types
import Data.Aeson.Safe (SafeJSON)
import qualified Data.Aeson.Safe as JSON
import System.FileLock
import UnliftIO.Directory
import Data.Aeson.Encode.Pretty (encodePretty)
import RIO (MonadUnliftIO(..))

_CONFIG_PATH :: Path Rel
_CONFIG_PATH = fromList ["config.json"]

createConfigIfMissing :: MonadCodchi m => m FilePath
createConfigIfMissing = do
    path <- getDriverPath DirConfig _CONFIG_PATH
    unlessM (doesFileExist path) $ do
        dir <- getDriverPath DirConfig emptyPath
        createDirectoryIfMissing True dir
        liftIO $ JSON.encodeFile path defaultConfig
    return path

tryReadConfigOrBackup :: (MonadIO m, SafeJSON a) => FilePath -> m (Maybe a)
tryReadConfigOrBackup path = do
    cfg <- liftIO $ JSON.decodeFileStrict' path

    when (isNothing cfg) $ do
        fsize <- getFileSize path
        when (fsize /= 0) $ do
            let bakPath = path <> ".bak"
            putTextLn $ "Could not parse config. Doing a backup of your old config to " <> toText bakPath
            putTextLn "No data is lost! Simple re-add your code machines and do a `codchi rebuild`."
            copyFile path bakPath

    return cfg

readConfig :: MonadCodchi m => m Config
readConfig = do
    path <- createConfigIfMissing
    liftIO $
        fromMaybe defaultConfig
            <$> withFileLock path Shared (\_ -> tryReadConfigOrBackup path)

modifyConfig :: MonadCodchi m => (Config -> m Config) -> m ()
modifyConfig f = do
    path <- createConfigIfMissing
    withRunInIO $ \run -> do
        -- we wait for exclusive access to config file
        withFileLock path Exclusive (const pass)
        -- Now the file is read. The first lock is already closed, because
        -- windows cant read from a locked file. This could result in a (highly
        -- improbable) race condition.
        cfg <- fromMaybe defaultConfig <$> tryReadConfigOrBackup path
        -- We reaquire the exclusive lock as soon as possible and run the
        -- possibly long running action
        cfg' <- withFileLock path Exclusive (\_ -> run $ f cfg)
        -- For writing the second lock has to be closed as well on windows
        writeFileLBS path $ encodePretty (JSON.safeToJSON cfg')


----------------------
-- Helper functions --
----------------------
