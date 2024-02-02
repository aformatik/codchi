{-# LANGUAGE CPP #-}

module Codchi.Platform (
    module X,
    module Codchi.Platform.CodchiMonad,
) where

#if defined(mingw32_HOST_OS)
import Codchi.Platform.Windows as X
#elif (darwin_HOST_OS)
import Codchi.Platform.Darwin as X
#else
import Codchi.Platform.Linux as X
#endif

import Codchi.Platform.CodchiMonad
