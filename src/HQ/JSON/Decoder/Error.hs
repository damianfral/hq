{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | What can go wrong while decoding the streaming JSON grammar.
module HQ.JSON.Decoder.Error where

import Data.Text qualified as T
import Relude hiding (Compose, id, many, some, state)

data DecodeError
  = UnexpectedEnd
  | UnexpectedChar Char
  | ExpectedColon
  | ExpectedCommaOrEnd
  | ExpectedObjectKey
  | InvalidEscape Char
  | InvalidUnicodeEscape
  | InvalidSurrogatePair
  | InvalidNumber Text
  | InvalidKeyword Text
  | TrailingInput
  deriving (Eq, Show)

-- | Render a decode error for the CLI boundary; nothing else renders
-- decode errors. Prose only: 'show' leaks constructors like
-- @InvalidKeyword "null"@ to users.
renderDecodeError :: DecodeError -> Text
renderDecodeError err = case err of
  UnexpectedEnd -> "unexpected end of JSON input"
  UnexpectedChar c -> "unexpected character " <> show c <> " in JSON input"
  ExpectedColon -> "expected ':' after an object key"
  ExpectedCommaOrEnd -> "expected ',' or the end of an object or array"
  ExpectedObjectKey -> "expected an object key"
  InvalidEscape c ->
    "invalid escape sequence '\\" <> T.singleton c <> "' in a JSON string"
  InvalidUnicodeEscape -> "invalid unicode escape sequence in a JSON string"
  InvalidSurrogatePair -> "invalid surrogate pair in a JSON string"
  InvalidNumber t
    | T.null t -> "invalid JSON number"
    | otherwise -> "invalid JSON number " <> t
  InvalidKeyword kw
    | T.null kw -> "expected a JSON value"
    | otherwise -> "expected JSON keyword " <> kw
  TrailingInput -> "trailing input after a JSON value"
