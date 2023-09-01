{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Codchi.Nix where

import Codchi.Config
import Codchi.Platform.CodchiMonad (DriverMeta (..))
import Codchi.Types
import qualified Data.Map.Strict as Map
import Text.Builder
import Development.GitRev (gitHash, gitBranch)
import Prelude hiding (intercalate)

-------------
-- Nix DSL --
-------------

data Nix
    = NIdent Text
    | NString Text
    | NBool Bool
    | NList [Nix]
    | NRecord (Map Text Nix)
    | NLambda Text Nix
    | NApp Nix Nix
    | NBinApp BinOp Nix Nix

instance IsString Nix where fromString = NIdent . toText

record :: [(Text, Nix)] -> Nix
record = NRecord . fromList

str :: Text -> Nix
str = NString . toText

list :: Foldable t => t Nix -> Nix
list = NList . toList

(|=|) :: Text -> Nix -> (Text, Nix)
(|=|) = (,)
infixr 0 |=|

(|:|) :: Text -> Nix -> Nix
(|:|) = NLambda
infixr 1 |:|

(|$|) :: Nix -> Nix -> Nix
(|$|) = NApp
infixr 2 |$|

data BinOp = ConcatStrings | ConcatLists | MergeAttrs

(|+|) :: Nix -> Nix -> Nix
(|+|) = NBinApp ConcatStrings

(|++|) :: Nix -> Nix -> Nix
(|++|) = NBinApp ConcatLists

(|//|) :: Nix -> Nix -> Nix
(|//|) = NBinApp MergeAttrs

builder :: Nix -> Builder
builder (NIdent i) = text i
builder (NString s) = surround '"' '"' (text s)
builder (NBool b) = if b then "true" else "false"
builder (NList elems) =
    surround '[' ']' (mconcat (map buildElem elems))
  where
    buildElem e = mconcat [indent, surround '(' ')' (builder e), char '\n']
builder (NRecord attrs) = surround '{' '}' (mconcat (map buildBinding (Map.toList attrs)))
  where
    buildBinding (name, val) = mconcat [indent, text name, " = ", builder val, ";\n"]
builder (NLambda arg body) =
    mconcat [text arg, ": ", builder body]
builder (NApp f arg) =
    mconcat
        [ surround '(' ')' (builder f)
        , char ' '
        , surround '(' ')' (builder arg)
        ]
builder (NBinApp op a b) =
    mconcat
        [ surround '(' ')' (builder a)
        , case op of
            ConcatStrings -> char '+'
            ConcatLists -> "++"
            MergeAttrs -> "//"
        , surround '(' ')' (builder b)
        ]

indent :: Builder
indent = mempty -- "  "

surround :: Char -> Char -> Builder -> Builder
surround start end b = char start <> b <> char end

surroundLn :: Char -> Char -> Builder -> Builder
surroundLn start end b = char start <> char '\n' <> b <> char end

-- intercalate :: Builder -> [Builder] -> Builder
-- intercalate _ [] = mempty
-- intercalate _ [x] = x
-- intercalate sep (x : xs) = x <> sep <> intercalate sep xs

----------------------
-- Flake generation --
----------------------

mkNixFlake :: InstanceConfig -> NixpkgsFollows -> DriverMeta -> Nix
mkNixFlake i follows meta =
    record
        [ "description"
            |=| str
                ( "Automatically generated flake for code machine "
                    <> i.name.text
                    <> ". DO NOT EDIT MANUALLY!"
                )
        , "inputs"
            |=| record
                ( ["codchi.url" |=| fromString _CODCHI_FLAKE_URL]
                    <> map toInput (Map.elems i.modules)
                    <> ["nixpkgs.follows" |=| str (nixpkgsFollows <> "/nixpkgs")]
                )
        , "outputs"
            |=| "inputs"
            |:| record
                [ "nixosConfigurations.default"
                    |=| "inputs.nixpkgs.lib.nixosSystem"
                    |$| record
                        [ "system" |=| str "x86_64-linux"
                        , "specialArgs.inputs" |=| ("inputs.codchi.inputs" |//| "inputs")
                        , "modules"
                            |=| list
                                ( map inputModule (Map.elems i.modules)
                                    <> [ record
                                            [ "codchi.internal.name" |=| str i.name.text
                                            , "codchi.internal." <> meta.moduleName <> ".enable" |=| NBool True
                                            ]
                                       , "inputs.codchi.nixosModules.default"
                                       ]
                                )
                        ]
                ]
        ]
  where
    toInput m =
        m.name.text
            |=| record
                [ "url" |=| str (toFlakeUrl m)
                , "flake" |=| NBool (isFlake m.moduleType)
                ]
    inputModule (m :: Module) =
        case m.moduleType of
            FlakeModule modName _dir ->
                NIdent ("inputs." <> m.name.text <> ".nixosModules." <> modName.text)
            LegacyModule path ->
                NIdent ("inputs." <> m.name.text) |+| str ("/" <> toUnixPath path.path)
    nixpkgsFollows = case follows of
        CodchiNixpkgs -> "codchi"
        ModuleNixpkgs m -> m.text

writeNixFile :: MonadIO m => FilePath -> Nix -> m ()
writeNixFile fp = writeFileText fp . run . builder

----------------------------------
-- Constants for codchi's flake --
----------------------------------

type Str s = (IsString s, Monoid s) => s

_GIT_COMMIT :: Str s
_GIT_COMMIT = $(gitHash)

_GIT_BRANCH :: Str s
_GIT_BRANCH = $(gitBranch)

-- FIXME
_CODCHI_FLAKE_URL :: Str s
_CODCHI_FLAKE_URL = "github:aformatik/codchi/" <> _GIT_COMMIT

codchiFlakeAttribute :: Str s -> Str s
codchiFlakeAttribute attr = mconcat [_CODCHI_FLAKE_URL, "#", attr]

-- codchiFlakePackage :: Str s -> Str s
-- codchiFlakePackage = codchiFlakeAttribute

ctrlRootfsCreateContents :: Str s -> Str s
ctrlRootfsCreateContents driver = codchiFlakeAttribute $ driver <> "-ctrl-rootfs.passthru.createContents"
