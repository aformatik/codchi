{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
module Util where

import Cleff
import Cleff.Error
import Control.Arrow        (left)
import Control.Exception    (throwIO)
import Data.Attoparsec.Text (Parser, parseOnly)

type family Errors es :: [Effect] where
    Errors (x ': xs) = (Error x ': Errors xs)
    Errors '[] = '[]

type family InterpretEffect es rest a where
    InterpretEffect (e : eRest) rest a = Eff (e : eRest ++ rest) a -> Eff (eRest ++ rest) a

class id :> es ++ rest => EffectInterpreter (id :: Effect) es rest where
    interpretEffect :: InterpretEffect es rest a

instance (Exception e, IOE :> es ++ rest) => EffectInterpreter IOE (Error e : es) rest where
    interpretEffect = either (liftIO . throwIO) return <=< runError

class PluckEffect (id :: Effect) effectsToPluck rest where
    pluckEffects :: Eff (effectsToPluck ++ rest) ~> Eff rest

instance (EffectInterpreter id (e:es) rest, PluckEffect id es rest) => PluckEffect id (e : es) rest where
    pluckEffects = pluckEffects @id @es . (interpretEffect @id @(e:es) @rest)

instance PluckEffect id '[] rest where
    pluckEffects = id

class ExceptionCollector e where
    collectException :: Exception ex => ex -> e

instance ( ExceptionCollector ex, Exception e, Error ex :> es ++ rest)
        => EffectInterpreter (Error ex) (Error e : es) rest where
    interpretEffect = fromEither . left (collectException @ex) <=< runError


class Parseable a where
    parser :: Parser a

    parse :: Text -> Either Text a
    parse = left toText . parseOnly parser

