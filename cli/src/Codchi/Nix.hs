{-# LANGUAGE StrictData #-}

module Codchi.Nix where

import Codchi.Types
import Codchi.Config
import qualified Data.Map.Strict as Map
import Data.Text.Builder.Linear
import Prelude hiding (intercalate)

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

data BinOp = ConcatStrings | ConcatLists

(|+|) :: Nix -> Nix -> Nix
(|+|) = NBinApp ConcatStrings

(|++|) :: Nix -> Nix -> Nix
(|++|) = NBinApp ConcatLists 
builder :: Nix -> Builder
builder (NIdent i) = fromText i
builder (NString s) = surround '"' '"' (fromText s)
builder (NBool b) = if b then "true" else "false"
builder (NList elems) =
    surround '[' ']' (mconcat (map buildElem elems))
  where
    buildElem e = mconcat [indent, surround '(' ')' (builder e), fromChar '\n']
builder (NRecord attrs) = surround '{' '}' (mconcat (map buildBinding (Map.toList attrs)))
  where
    buildBinding (name, val) = mconcat [indent, fromText name, " = ", builder val, ";\n"]
builder (NLambda arg body) =
    mconcat [fromText arg, ": ", builder body]
builder (NApp f arg) =
    mconcat
        [ surround '(' ')' (builder f)
        , fromChar ' '
        , surround '(' ')' (builder arg)
        ]
builder (NBinApp op a b) =
    mconcat
        [ surround '(' ')' (builder a)
        , case op of
            ConcatStrings -> fromChar '+'
            ConcatLists -> "++"
        , surround '(' ')' (builder b)
        ]

indent :: Builder
indent = "  "

surround :: Char -> Char -> Builder -> Builder
surround start end b = fromChar start <> b <> fromChar end

surroundLn :: Char -> Char -> Builder -> Builder
surroundLn start end b = fromChar start <> fromChar '\n' <> b <> fromChar end

intercalate :: Builder -> [Builder] -> Builder
intercalate _ [] = mempty
intercalate _ [x] = x
intercalate sep (x : xs) = x <> sep <> intercalate sep xs

mkNixFlake :: InstanceConfig -> NixpkgsFollows -> Text -> Nix
mkNixFlake i follows driverModule =
    record
        [ "description"
            |=| str
                ( "Automatically generated flake for code machine "
                    <> i.name.text
                    <> ". DO NOT EDIT MANUALLY!"
                )
        , "inputs"
            |=| record
                ( ["codchi.url" |=| "github:aformatik/codchi"]
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
                        , "specialArgs.inputs" |=| "inputs"
                        , "modules"
                            |=| list
                                ( [ record ["codchi.instance.name" |=| str i.name.text]
                                  , NIdent ("inputs.codchi.nixosModules." <> driverModule)
                                  ]
                                    <> map inputModule (Map.elems i.modules)
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
writeNixFile fp = writeFileBS fp . runBuilderBS . builder
