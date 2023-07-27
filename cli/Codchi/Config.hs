{-# LANGUAGE TypeOperators #-}

module Codchi.Config where

import Cleff
import Codchi.Dsl
import Codchi.Types
import Data.Aeson (FromJSON)
import qualified Data.Aeson as JSON
import System.FileLock
import UnliftIO.Directory

_CONFIG_PATH :: Path Rel
_CONFIG_PATH = fromList ["config.json"]

createConfigIfMissing :: [CodchiL, IOE] :>> es => Eff es FilePath
createConfigIfMissing = do
    path <- getDriverPath DirConfig _CONFIG_PATH
    unlessM (doesFileExist path) $ do
        dir <- getDriverPath DirConfig emptyPath
        createDirectoryIfMissing True dir
        liftIO $ JSON.encodeFile path defaultConfig
    return path

tryReadConfigOrBackup :: (MonadIO m, FromJSON a) => FilePath -> m (Maybe a)
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

readConfig :: [CodchiL, IOE] :>> es => Eff es Config
readConfig = do
    path <- createConfigIfMissing
    liftIO $
        fromMaybe defaultConfig
            <$> withFileLock path Shared (\_ -> tryReadConfigOrBackup path)

modifyConfig ::
    [CodchiL, IOE] :>> es =>
    (Config -> Eff es Config) ->
    Eff es ()
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
        JSON.encodeFile path cfg'
