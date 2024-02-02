module Codchi.Parser where

import Codchi.Error
import qualified Control.Exception.Annotated.UnliftIO as Ann
import Data.Aeson
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Attoparsec.Text (Parser, parseOnly, takeText)
import Options.Applicative (ReadM, eitherReader)
import RIO (MonadThrow)

class Parseable a where
    parser :: Parser a
    parser = either (fail . toString) return . parse =<< takeText
    parse :: Text -> Either String a
    parse = parseOnly parser

newtype ParseableFromJSON a = ParseableFromJSON a

instance Parseable a => FromJSON (ParseableFromJSON a) where
    parseJSON (JSON.String s) = case parse s of
        Right x -> pure (ParseableFromJSON x)
        Left err -> JSON.parseFail ("parsing failed for " <> toString s <> " with error: " <> err)
    parseJSON invalid =
        JSON.prependFailure "parsing instance of parseable failed, " (JSON.typeMismatch "String" invalid)

parseable :: Parseable p => ReadM p
parseable = eitherReader $ parseOnly parser . toText

parse_ :: (Parseable a, MonadIO m, MonadThrow m) => Text -> m a
parse_ txt = case parse txt of
    Left err -> Ann.throw $ InternalParseError txt err
    Right r -> return r
