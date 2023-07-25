{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Codchi.Effects where

import Cleff
import Cleff.Error
import Codchi.Parser
import Control.Arrow (left)
import Control.Exception (throwIO)

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

instance (EffectInterpreter id (e : es) rest, PluckEffect id es rest) => PluckEffect id (e : es) rest where
    pluckEffects = pluckEffects @id @es . (interpretEffect @id @(e : es) @rest)

instance PluckEffect id '[] rest where
    pluckEffects = id

class ExceptionCollector e where
    collectException :: Exception ex => ex -> e

instance
    (ExceptionCollector ex, Exception e, Error ex :> es ++ rest) =>
    EffectInterpreter (Error ex) (Error e : es) rest
    where
    interpretEffect = fromEither . left (collectException @ex) <=< runError

newtype UserError
    = UserError Text
    deriving (Show)
    deriving anyclass (Exception)

newtype NixError
    = NixError Text
    deriving (Show)
    deriving anyclass (Exception)

newtype UnrecoverableError
    = UnrecoverableError Text
    deriving (Show)
    deriving anyclass (Exception)

data DriverException = forall e. Exception e => DriverException e
    deriving (Exception)
deriving instance Show DriverException

instance ExceptionCollector DriverException where
    collectException = DriverException

data Panic = forall e. Exception e => Panic e
    deriving (Exception)
deriving instance Show Panic

instance ExceptionCollector Panic where
    collectException = Panic

rethrowPanic ::
    forall e catchAllErr errs rest a.
    ( catchAllErr ~ Error Panic
    , catchAllErr :> rest
    , Exception e
    , errs ~ Errors '[e]
    ) =>
    Eff (Error e : rest) a ->
    Eff rest a
rethrowPanic = pluckEffects @catchAllErr @errs

-- | Parseable failure
newtype ParseException
    = ParseException Text
    deriving (Show)
    deriving anyclass (Exception)

parse_ :: Parseable a => Error ParseException :> es => Text -> Eff es a
parse_ = either (throwError . ParseException) return . parse
