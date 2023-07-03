module Types
    ( Abs
    , DevenvInstance (..)
    , DevenvStatus (..)
    , DriverException (..)
      -- , FlakeURL (unFlakeURL)
    , DesktopEntry (..)
    , DevenvFlake
    , InstanceName (getInstanceName)
    , InstanceStatus (..)
    , NixError (..)
    , NixExpr
    , Panic (..)
    , ParseException (..)
    , Path
    , Rel
    , StorePath (unStorePath)
    , UnrecoverableError (..)
    , UserError (..)
    , _DEVENV_APP_NAME
    , _DEVENV_CONTROLLER
    , _DEVENV_CONTROLLER_DIR
    , _DEVENV_INSTANCE_PREFIX
    , _flakeMetadata
    , _flakeURL
    , _moduleName
    , addNamesFromURL
    , fromPathSegments
    , getInstanceNameWithPrefix
    , joinPaths
    , mkNTPath
    , mkUnixPath
    , parseDesktopEntry
    , parse_
    , rethrowPanic
    , toNTPath
    , toUnixPath
    ) where

import           Cleff
import           Cleff.Error          (Error, throwError)
import           Control.Arrow        (left)
import           Data.Attoparsec.Text as P (char, eitherP, endOfInput, endOfLine, inClass,
                                            isEndOfLine, many', parseOnly, sepBy, skipWhile, string,
                                            takeWhile, takeWhile1, (<?>))
import           Data.Char            (isSpace)
import           Data.Data
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T
import qualified Relude.Unsafe        as Unsafe
import           Util

_DEVENV_INSTANCE_PREFIX, _DEVENV_CONTROLLER_DIR, _DEVENV_CONTROLLER, _DEVENV_APP_NAME :: IsString s => Semigroup s => s
_DEVENV_APP_NAME = "devenv"
_DEVENV_CONTROLLER = _DEVENV_APP_NAME <> "_controller"
_DEVENV_CONTROLLER_DIR = "controller"
_DEVENV_INSTANCE_PREFIX = _DEVENV_APP_NAME <> "_instance_"

type NixExpr = Text

newtype InstanceName
    = InstanceName { getInstanceName :: Text }
  deriving (Eq, Show)

getInstanceNameWithPrefix :: InstanceName -> Text
getInstanceNameWithPrefix (InstanceName n) = _DEVENV_INSTANCE_PREFIX <> n

instance Parseable InstanceName where
    parser = InstanceName
          <$> (P.takeWhile (inClass pat) <* endOfInput)
          <?> "Instance name must match pattern " <> show pat
        where pat = "a-zA-Z0-9_-"

data DevenvFlake meta = DevenvFlake
    { _flakeURL      :: Text
    , _moduleName    :: Text
    , _flakeMetadata :: meta
        -- ^ Can be input name in flake.nix
    }
  deriving (Eq, Show)

instance Parseable (DevenvFlake ()) where
    parser = DevenvFlake
          <$> (P.takeWhile1 (inClass flakePat) <?> "flake url must match " <> show flakePat)
          <*  P.char '#'
          <*> (P.takeWhile1 (inClass modulePat) <?> "module name must match " <> show modulePat)
          <*> pass
          <*  P.endOfInput
        where
            flakePat = "a-zA-Z0-9/:?=%&_-"
            modulePat = "a-zA-Z0-9_-"

addNamesFromURL :: [DevenvFlake ()] -> [DevenvFlake Text]
addNamesFromURL flakes = map addName flakes where
    lastMaybe xs | null xs   = Nothing
                 | otherwise = Just $ Unsafe.last xs
    urlToName (i::Int) = fromMaybe ("flake-" <> show i)
                       . lastMaybe
                       . T.split (== '/')
    indexed = Map.fromList
            $ zipWith
                (\flake i -> (_flakeURL flake, urlToName i $ _flakeURL flake))
                flakes [1..]
    addName DevenvFlake{..} = DevenvFlake{_flakeMetadata = indexed Map.! _flakeURL, ..}
    -- firstInput = urlToName 1 . _flakeURL <$> listToMaybe inputs

newtype StorePath
    = StorePath { unStorePath :: Path Rel }
  deriving (Eq, Show)

instance Parseable StorePath where
    parser = StorePath . mkUnixPath @Rel
          <$> ( (<>)
             <$> string "/nix/store/"
             <*> P.takeWhile (inClass pat)
             <* P.skipWhile P.isEndOfLine
             <* endOfInput
              )
          <?> "Store path must match pattern " <> show pat
        where pat = "a-zA-Z0-9/._-"

data InstanceStatus = Running | Stopped deriving (Eq, Show)

data DevenvInstance = DevenvInstance
    { _name   :: !InstanceName
    , _status :: !InstanceStatus
    }
  deriving (Show)

data DevenvStatus
    = DevenvInstalled InstanceStatus
    | DevenvNotInstalled
  deriving (Show)

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

rethrowPanic
    :: forall e catchAllErr errs rest a.
     ( catchAllErr ~ Error Panic
     , catchAllErr :> rest
     , Exception e
     , errs ~ Errors '[ e ] )
    => Eff (Error e : rest) a
    -> Eff rest a
rethrowPanic = pluckEffects @catchAllErr @errs


-- | Parseable failure
newtype ParseException
    = ParseException Text
  deriving (Show)
  deriving anyclass (Exception)

parse_ :: Parseable a => Error ParseException :> es => Text -> Eff es a
parse_ = either (throwError . ParseException) return . parse

data Abs deriving (Typeable)
data Rel deriving (Typeable)

newtype Path t
    = Path [Text]
  deriving (Eq, Show)
  deriving newtype (Semigroup)

mkUnixPath :: Text -> Path t
mkUnixPath = Path . filter (not . T.null) . T.splitOn "/"

mkNTPath :: Text -> Path t
mkNTPath = Path . filter (not . T.null) . T.splitOn "\\"

fromPathSegments :: [Text] -> Path t
fromPathSegments = Path

toUnixPath :: forall t. Typeable t => Path t -> Text
toUnixPath (Path p) = prefix <> T.intercalate "/" p
    where
        prefix = case eqT @t @Abs of
            Just Refl -> "/"
            Nothing   -> ""

toNTPath :: Path t -> Text
toNTPath (Path p) = T.intercalate "\\" p

class Joinable t u | t -> u where
    joinPaths :: Path t -> Path u -> Path t
    joinPaths (Path p) (Path q) = Path $ p <> q

instance Joinable Abs Rel where
instance Joinable Rel Rel where

data DesktopEntry icon = DesktopEntry
    { deName     :: Text
    , deIcon     :: icon
    , deExec     :: Text
    , deTerminal :: Bool
    }
  deriving (Show)

parseDesktopEntry :: Text -> Either Text (DesktopEntry ())
parseDesktopEntry txt =
    let p = do
            let header = P.string "[Desktop Entry]" >> P.skipWhile isSpace
                comment = P.skipWhile isSpace
                       >> P.char '#'
                       >> P.skipWhile (not . P.isEndOfLine)
                attr = (,)
                    <$> (P.takeWhile1 (/= '=') <* P.char '=')
                    <*> (T.strip <$> P.takeWhile1 (\c -> not (P.isEndOfLine c) && c /= '#'))
                     <* P.skipWhile (not . P.isEndOfLine)
            void $ P.sepBy comment P.endOfLine
            header
            rights <$> P.sepBy (P.eitherP comment attr) P.endOfLine
    in do
        deAttrs <- left toText $ P.parseOnly p txt

        let getAttr attrName = snd <$> find ((== attrName) . fst) deAttrs

        deName <- "Missing 'Name' entry" `maybeToRight` getAttr "Name"
        -- first try 'TryExec' since it doesn't include %F / %U
        deExec <- maybeToRight "Missing 'Exec' entry" $
            getAttr "TryExec" <|> getAttr "Exec"
        let deIcon     = ()
            deTerminal = case getAttr "Terminal" of
                Just "true" -> True
                _           -> False

        return $ DesktopEntry {..}
