{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | What can go wrong while decoding the streaming JSON grammar.
module HQ.JSON.Decoder.Error where

import Relude hiding (Compose, id, many, some, state)

data DecodeError
  = UnexpectedEnd
  | UnexpectedChar Char
  | UnexpectedToken Text
  | ExpectedColon
  | ExpectedCommaOrEnd
  | ExpectedObjectKey
  | ExpectedValue
  | InvalidEscape Char
  | InvalidUnicodeEscape
  | InvalidSurrogatePair
  | InvalidNumber Text
  | InvalidKeyword Text
  | TrailingInput
  deriving (Eq, Show)
