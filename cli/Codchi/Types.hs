{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Codchi.Types where

import Codchi.Parser
import Control.Arrow (left)
import Data.Aeson (FromJSON, ToJSON (toJSON))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Attoparsec.Text as P
import Data.Char (isSpace)
import Data.Data
import qualified Data.Text as T
import GHC.Exts (IsList (..))
import GHC.Records

_INSTANCE_PREFIX, _CONTROLLER_DIR, _CONTROLLER, _APP_NAME :: IsString s => Semigroup s => s
_APP_NAME = "codchi"
_CONTROLLER = _APP_NAME <> "-controller"
_CONTROLLER_DIR = "controller"
_INSTANCE_PREFIX = _APP_NAME <> "-instance-"

type NixExpr = Text

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
            <$> (P.takeWhile (inClass pat) <* endOfInput)
            <?> "Name must match pattern " <> show pat
      where
        pat = "a-zA-Z0-9_-"

newtype StorePath = StorePath {path :: Path Rel}
    deriving (Eq, Show)

instance Parseable StorePath where
    parser =
        StorePath . mkUnixPath @Rel
            <$> ( (<>)
                    <$> string "/nix/store/"
                    <*> P.takeWhile (inClass pat)
                    <* P.skipWhile P.isEndOfLine
                    <* endOfInput
                )
            <?> "Store path must match pattern " <> show pat
      where
        pat = "a-zA-Z0-9/._-"

data InstanceStatus
    = NotInstalled
    | Stopped
    | Running
    deriving (Eq, Show)

data CodchiInstance = CodchiInstance
    { name :: CodchiName
    , status :: InstanceStatus
    }
    deriving (Show)

data CodchiStatus
    = CodchiInstalled InstanceStatus
    | CodchiNotInstalled
    deriving (Show)

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
                deAttrs <- left toText $ P.parseOnly p txt

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

newtype Config = Config
    { instances :: Map Text InstanceConfig
    }
    deriving (Eq, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

defaultConfig :: Config
defaultConfig = Config mempty

data InstanceConfig = InstanceConfig
    { name :: CodchiName
    , nixpkgsFollows :: Maybe NixpkgsFollows
    , modules :: Map Text Module
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

defaultInstance :: CodchiName -> InstanceConfig
defaultInstance name = InstanceConfig name Nothing mempty

data NixpkgsFollows
    = CodchiNixpkgs
    | ModuleNixpkgs CodchiName
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Module = Module
    { name :: CodchiName
    , uri :: ModuleUri
    , rev :: Maybe Text
    , moduleType :: ModuleType
    }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ModuleUri
    = LocalModule (Path Abs)
    | GitModule GitModuleConfig
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data GitModuleConfig = GitModuleConfig
    { protocol :: GitProtocol
    , uri :: Text
    -- ^ must be http / ssh URL without protocol and leading :// and no trailing /.
    -- Syntax: [user[:password]@]hostname.tld[/path]
    }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Parseable GitModuleConfig where
    parser =
        (GitModuleConfig GitHttps <$> httpParser)
            <|> (GitModuleConfig GitSsh <$> sshParser)
      where
        authChars = "a-zA-Z0-9_.+~=%-"
        httpUrlChars = authChars <> ":/?#"
        basicAuth = do
            user <- P.takeWhile1 (P.inClass authChars)
            pwd <-
                option "" $
                    (<>) <$> ":" <*> P.takeWhile1 (P.inClass authChars)
            _ <- P.char '@'
            return (user <> pwd <> "@")
        httpParser = do
            _ <- "http" >> optional (char 's') >> "://"
            auth <- option "" basicAuth
            host <- P.takeWhile (P.inClass httpUrlChars)
            return (auth <> host)

        isHostSep c = c == ':' || c == '/'
        sshUrlChars = authChars <> ":/"
        sshParser = do
            _ <- optional "ssh://"
            auth <- basicAuth
            host <- P.takeWhile (not . isHostSep)
            _ <- P.satisfy isHostSep
            rest <- P.takeWhile (P.inClass sshUrlChars)
            return (auth <> host <> "/" <> rest)

data GitProtocol = GitHttps | GitSsh
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

toFlakeUrl :: Module -> Text
toFlakeUrl m =
    let rev = maybe "" ("/" <>) m.rev
     in case m.uri of
            LocalModule p -> "path:" <> toUnixPath p <> rev
            GitModule g ->
                let prot = case g.protocol of
                        GitHttps -> "https"
                        GitSsh -> "ssh"
                 in "git+" <> prot <> "://" <> g.uri <> rev

data ModuleType
    = FlakeModule CodchiName
    | LegacyModule NixFilePath
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

isFlake :: ModuleType -> Bool
isFlake (FlakeModule _) = True
isFlake (LegacyModule _) = False

newtype NixFilePath = NixFilePath {path :: Path Rel}
    deriving newtype (Eq, Show, ToJSON, FromJSON)

instance Parseable NixFilePath where
    parse input =
        if ".nix" `T.isSuffixOf` input
            then Right $ NixFilePath (mkUnixPath input)
            else Left $ show input <> " is not a path to a nix file"

-- instance Parseable HttpBasicAuth where
--     parser =
--         HttpBasicAuth
--             <$> P.takeWhile (P.inClass pat)
--             <* P.char ':'
--             <*> P.takeWhile (P.inClass pat)
--             <* P.endOfInput
--             <?> "Http basic auth must have syntax \"USER:PASSWORD\" where both USER and PASSWORD match " <> show pat
--       where
--         pat = "a-zA-Z0-9_-"

-- instance Parseable (CodchiFlake ()) where
--     parser =
--         CodchiFlake
--             <$> (P.takeWhile1 (inClass flakePat) <?> "flake url must match " <> show flakePat)
--             <* P.char '#'
--             <*> (P.takeWhile1 (inClass modulePat) <?> "module name must match " <> show modulePat)
--             <*> pass
--             <* P.endOfInput
--       where
--         flakePat = "a-zA-Z0-9/:?=%&_-"
--         modulePat = "a-zA-Z0-9_-"

-- addNamesFromURL :: [CodchiFlake ()] -> [CodchiFlake Text]
-- addNamesFromURL flakes = map addName flakes
--   where
--     lastMaybe xs
--         | null xs = Nothing
--         | otherwise = Just $ Unsafe.last xs
--     urlToName (i :: Int) =
--         fromMaybe ("flake-" <> show i)
--             . lastMaybe
--             . T.split (== '/')
--     indexed =
--         Map.fromList $
--             zipWith
--                 (\flake i -> (flake.url, urlToName i flake.url))
--                 flakes
--                 [1 ..]
--     addName CodchiFlake{..} = CodchiFlake{metadata = indexed ! url, ..}

-- firstInput = urlToName 1 . _flakeURL <$> listToMaybe inputs
