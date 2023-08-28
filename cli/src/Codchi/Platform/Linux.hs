{-# OPTIONS_GHC -Wno-orphans #-}

module Codchi.Platform.Linux where

import Codchi.Config
import Codchi.Platform.CodchiMonad
import Codchi.Types
import RIO (HasLogFunc, RIO)
import UnliftIO.Directory

instance MonadCodchi (RIO Codchi) where
    getDriverPath dirType s =
        toString . toUnixPath . (</> s)
            <$> case dirType of
                DirCtrl -> undefined
                DirState -> getOrCreateXdgDir XdgState ""
                DirConfig -> getOrCreateXdgDir XdgConfig ""

instance IsString (Path x) where
    fromString = mkUnixPath . toText

getOrCreateXdgDir :: HasLogFunc e => XdgDirectory -> Path Rel -> RIO e (Path Abs)
getOrCreateXdgDir xdg dir = do
    path <- fromString <$> getXdgDirectory xdg _APP_NAME
    let innerPath = path </> dir
    fp <- logTraceId "create-dir" (toString $ toUnixPath innerPath)
    createDirectoryIfMissing True fp
    return innerPath
