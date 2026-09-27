{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Keyword parsing (null/true/false) for the streaming JSON decoder.
module HQ.JSON.Decoder.Keyword where

import qualified Data.Text as T
import HQ.JSON.Decoder.Core
import HQ.JSON.Decoder.Error (DecodeError (..))
import HQ.JSON.Event (JSONEvent (..))
import Relude hiding (Compose, id, many, some, state)

startKeyword :: DecoderState -> Text -> Text -> Int -> Either DecodeError DecoderResult
startKeyword decoder input keyword consumed =
  let state = keywordState keyword consumed
   in stepKeyword
        decoder {decoderInput = input, decoderPhase = DecoderPhaseKeyword state}
        state

keywordState :: Text -> Int -> KeywordState
keywordState keyword n
  | keyword == "null" = KeywordNull n
  | keyword == "true" = KeywordTrue n
  | keyword == "false" = KeywordFalse n
  | otherwise = KeywordNull n

stepKeyword :: DecoderState -> KeywordState -> Either DecodeError DecoderResult
stepKeyword decoder state =
  if index == T.length keyword
    then case T.uncons $ decoderInput decoder of
      Nothing -> finalizeKeyword decoder
      Just (c, _) | isJsonDelimiter c -> finalizeKeyword decoder
      Just _ -> Left (InvalidKeyword keyword)
    else case T.uncons (decoderInput decoder) of
      Nothing ->
        Right $ NeedInput decoder {decoderPhase = DecoderPhaseKeyword state}
      Just (c, rest)
        | c == T.index keyword index ->
            stepKeyword
              decoder
                { decoderInput = rest,
                  decoderPhase = DecoderPhaseKeyword (advanceKeyword state)
                }
              (advanceKeyword state)
      _ -> Left (InvalidKeyword keyword)
  where
    (keyword, index) = case state of
      KeywordNull i -> ("null", i)
      KeywordTrue i -> ("true", i)
      KeywordFalse i -> ("false", i)

advanceKeyword :: KeywordState -> KeywordState
advanceKeyword state = case state of
  KeywordNull n -> KeywordNull (n + 1)
  KeywordTrue n -> KeywordTrue (n + 1)
  KeywordFalse n -> KeywordFalse (n + 1)

finalizeKeyword :: DecoderState -> Either DecodeError DecoderResult
finalizeKeyword decoder = case decoderPhase decoder of
  DecoderPhaseKeyword state -> do
    let (keyword, index) = case state of
          KeywordNull i -> ("null", i)
          KeywordTrue i -> ("true", i)
          KeywordFalse i -> ("false", i)
    if index == T.length keyword
      then do
        let event = case state of
              KeywordNull _ -> JSONNull
              KeywordTrue _ -> JSONBool True
              KeywordFalse _ -> JSONBool False
        emitScalar event (decoderInput decoder) decoder
      else Left $ InvalidKeyword keyword
  _ -> Left $ InvalidKeyword mempty
