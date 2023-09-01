{-# LANGUAGE DerivingVia #-}

module Codchi.Config.Common where

import Codchi.Parser (Parseable (..), ParseableFromJSON (..))
import Codchi.Types
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.Attoparsec.Text ((<?>))
import qualified Data.Attoparsec.Text as P
import GHC.Records

_INSTANCE_PREFIX, _CONTROLLER_DIR, _CONTROLLER, _APP_NAME :: IsString s => Semigroup s => s
_APP_NAME = "codchi"
_CONTROLLER = _APP_NAME <> "-controller"
_CONTROLLER_DIR = "controller"
_INSTANCE_PREFIX = _APP_NAME <> "-instance-"

---------------------------
-- Data types for codchi --
---------------------------

data DirectoryType
    = -- | Used for logs
      DirState
    | -- | Used for config.json
      DirConfig
    | -- | Root directory of controller. Should contain at least:
      -- - /instances (code machine flakes)
      -- - /nix
      DirCtrl

newtype CodchiName = CodchiName {text :: Text}
    deriving (Eq, Show, Generic)
    deriving (FromJSON) via (ParseableFromJSON CodchiName)

instance HasField "withPrefix" CodchiName Text where
    getField i = _INSTANCE_PREFIX <> i.text

instance ToJSON CodchiName where
    toJSON i = JSON.String i.text

instance Parseable CodchiName where
    parser =
        CodchiName
            <$> (P.takeWhile (P.inClass pat) <* P.endOfInput)
            <?> "Name must match pattern " <> show pat
      where
        pat = "a-zA-Z0-9_-"

data MachineStatus
    = MachineNotInstalled
    | MachineStopped
    | MachineRunning
    | MachineOrphaned
    deriving (Eq, Show)

data CodeMachine = CodeMachine
    { name :: CodchiName
    , status :: MachineStatus
    }
    deriving (Show, Eq)

data CodchiStatus
    = CodchiNotInstalled
    | CodchiStopped
    | CodchiRunning
    deriving (Show, Eq)

newtype StorePath = StorePath {path :: Path Rel}
    deriving (Eq, Show)

instance Parseable StorePath where
    parser =
        StorePath . mkUnixPath @Rel
            <$> ( (<>)
                    <$> P.string "/nix/store/"
                    <*> P.takeWhile (P.inClass pat)
                    <* P.skipWhile P.isEndOfLine
                    <* P.endOfInput
                )
            <?> "Store path must match pattern " <> show pat
      where
        pat = "a-zA-Z0-9/._-"
