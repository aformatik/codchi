{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}

module Codchi.Config.V012 where

import Codchi.Parser
import Data.Aeson (FromJSON, ToJSON)
import Data.Attoparsec.Text as P
import qualified Data.Text as T
import Codchi.Types
import Codchi.Config.Common (CodchiName)
import Data.Aeson.Safe (SafeJSON(..), noVersion, base)

instance SafeJSON Config where
    version = noVersion
    kind = base

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
    , branchCommit :: Maybe BranchCommit
    , moduleType :: ModuleType
    }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data BranchCommit = BranchCommit
    { branch :: Text
    , commit :: Maybe Text
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
    let urlPath = case m.uri of
            LocalModule p -> "path:" <> toUnixPath p
            GitModule g ->
                let prot = case g.protocol of
                        GitHttps -> "https"
                        GitSsh -> "ssh"
                 in "git+" <> prot <> "://" <> g.uri
        query =
            [ ("ref", m.branchCommit <&> (.branch))
            , ("rev", m.branchCommit >>= (.commit))
            ,
                ( "dir"
                , case m.moduleType of
                    FlakeModule _ d -> toUnixPath . (.path) <$> d
                    _ -> Nothing
                )
            ]
                & mapMaybe (\(key, val) -> (key,) <$> val)
                & \case
                    [] -> ""
                    params ->
                        params
                            & map (\(name, val) -> name <> "=" <> val)
                            & \p ->
                                (if '?' `T.elem` urlPath then "&" else "?")
                                    <> T.intercalate "&" p
     in urlPath <> query

data ModuleType
    = FlakeModule CodchiName (Maybe FlakeSubDir)
    | LegacyModule NixFilePath
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype FlakeSubDir = FlakeSubDir {path :: Path Rel}
    deriving newtype (Eq, Show, ToJSON, FromJSON)
instance Parseable FlakeSubDir where
    parse input =
        if "/" `T.isSuffixOf` input
            then Left "The flake directory must be relative to the git repository."
            else Right (FlakeSubDir $ mkUnixPath input)

isFlake :: ModuleType -> Bool
isFlake (FlakeModule _ _) = True
isFlake (LegacyModule _) = False

newtype NixFilePath = NixFilePath {path :: Path Rel}
    deriving newtype (Eq, Show, ToJSON, FromJSON)

instance Parseable NixFilePath where
    parse input =
        if ".nix" `T.isSuffixOf` input
            then Right $ NixFilePath (mkUnixPath input)
            else Left $ show input <> " is not a path to a nix file"
