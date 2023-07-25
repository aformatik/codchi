{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Codchi.Platform.Linux where

import Cleff
import Codchi.Dsl
import Codchi.Effects
import Codchi.Types
import UnliftIO.Directory

runCodchiLIO :: Interpreter CodchiL (Logger : IOE : Errors [DriverException, UserError, Panic])
runCodchiLIO = interpretIO $ \case
    GetDriverPath dirType s ->
        toString . toUnixPath . (</> s)
            <$> case dirType of
                DirCtrl -> undefined
                DirState -> getOrCreateXdgDir XdgState ""
                DirConfig -> getOrCreateXdgDir XdgConfig ""
    _ -> return undefined -- error "not implemented"

instance IsString (Path x) where
    fromString = mkUnixPath . toText

getOrCreateXdgDir :: MonadIO m => XdgDirectory -> Path Rel -> m (Path Abs)
getOrCreateXdgDir xdg dir = liftIO $ do
    path <- fromString <$> getXdgDirectory xdg _APP_NAME
    let innerPath = path </> dir
    createDirectoryIfMissing True (logTraceId "create-dir" $ toString $ toUnixPath innerPath)
    return innerPath
