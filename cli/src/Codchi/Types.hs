{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Codchi.Types where

import Codchi.Parser
import Data.Aeson (FromJSON, ToJSON (toJSON))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Attoparsec.Text as P
import Data.Char (isSpace)
import Data.Data
import qualified Data.Text as T
import GHC.Exts (IsList (..))

----------
-- Path --
----------

data Abs deriving (Typeable)
data Rel deriving (Typeable)

newtype Path t
    = Path [Text]
    deriving (Eq, Show)
    deriving newtype (Semigroup)

emptyPath :: Path t
emptyPath = Path []

instance FromJSON (Path p) where
    parseJSON (JSON.String p) = pure (mkUnixPath p)
    parseJSON invalid =
        JSON.prependFailure "parsing instance of parseable failed, " (JSON.typeMismatch "String" invalid)

instance Typeable p => ToJSON (Path p) where
    toJSON = JSON.String . toUnixPath

mkUnixPath :: Text -> Path t
mkUnixPath = Path . filter (not . T.null) . T.splitOn "/"

instance IsList (Path t) where
    type Item (Path t) = Text
    toList (Path p) = p
    fromList = Path

toUnixPath :: forall t. Typeable t => Path t -> Text
toUnixPath (Path p) = prefix <> T.intercalate "/" p
  where
    prefix = case eqT @t @Abs of
        Just Refl -> "/"
        Nothing -> ""

(</>) :: Path t -> Path Rel -> Path t
(Path p) </> (Path q) = Path $ p <> q

---------------------
-- Desktop entries --
---------------------

data DesktopEntry icon = DesktopEntry
    { name :: Text
    , icon :: icon
    , exec :: Text
    , isTerminal :: Bool
    }
    deriving (Show)

instance Parseable (DesktopEntry ()) where
    parse txt =
        let p = do
                let header = P.string "[Desktop Entry]" >> P.skipWhile isSpace
                    comment =
                        P.skipWhile isSpace
                            >> P.char '#'
                            >> P.skipWhile (not . P.isEndOfLine)
                    attr =
                        (,)
                            <$> (P.takeWhile1 (/= '=') <* P.char '=')
                            <*> (T.strip <$> P.takeWhile1 (\c -> not (P.isEndOfLine c) && c /= '#'))
                            <* P.skipWhile (not . P.isEndOfLine)
                void $ P.sepBy comment P.endOfLine
                header
                rights <$> P.sepBy (P.eitherP comment attr) P.endOfLine
         in do
                deAttrs <- P.parseOnly p txt

                let getAttr attrName = snd <$> find ((== attrName) . fst) deAttrs

                name <- "Missing 'Name' entry" `maybeToRight` getAttr "Name"
                -- first try 'TryExec' since it doesn't include %F / %U
                exec <-
                    maybeToRight "Missing 'Exec' entry" $
                        getAttr "TryExec" <|> getAttr "Exec"
                let icon = ()
                    isTerminal = case getAttr "Terminal" of
                        Just "true" -> True
                        _ -> False

                return $ DesktopEntry{..}
