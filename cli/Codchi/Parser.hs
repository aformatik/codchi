module Codchi.Parser where

import Control.Arrow (left)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Attoparsec.Text (Parser, parseOnly, takeText)
import Data.Aeson
import Options.Applicative (ReadM, eitherReader)

class Parseable a where
    parser :: Parser a
    parser = either (fail . toString) return . parse =<< takeText
    parse :: Text -> Either Text a
    parse = left toText . parseOnly parser

newtype ParseableFromJSON a = ParseableFromJSON a

instance Parseable a => FromJSON (ParseableFromJSON a) where
    parseJSON (JSON.String s) = case parse s of
        Right x -> pure (ParseableFromJSON x)
        Left err -> JSON.parseFail (toString $ "parsing failed for " <> s <> " with error: " <> err)
    parseJSON invalid =
        JSON.prependFailure "parsing instance of parseable failed, " (JSON.typeMismatch "String" invalid)

parseable :: Parseable p => ReadM p
parseable = eitherReader $ parseOnly parser . toText

