{-# LANGUAGE DeriveAnyClass #-}

module Codchi.Error where

import RIO (Show (..))

data CodchiError
    = CodchiUserError String
    | InternalNixError String
    | InternalParseError Text String
    | InternalDriverError String
    | InternalPanic String
    | forall e. Exception e => InternalUnknownError e
    deriving (Exception)

instance Show CodchiError where
    show = \case
        CodchiUserError msg -> msg
        InternalNixError msg -> "Error when invoking Nix: " <> msg
        InternalParseError txt msg ->
            intercalate
                "\n"
                [ "Error when parsing text: " <> msg
                , "Parsed text was: "
                , toString txt
                ]
        InternalDriverError msg -> "Error in platform driver of codchi: " <> msg
        InternalPanic msg -> "Panic: " <> msg
        InternalUnknownError e -> "Unknown error: " <> displayException e
