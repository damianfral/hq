{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Keyword parsing (null/true/false) for the streaming JSON decoder.
module HQ.JSON.Decoder.Keyword where

import Data.Text qualified as T
import HQ.JSON.Decoder.Core
import HQ.JSON.Decoder.Error (DecodeError (..))
import HQ.JSON.Event (JSONEvent (..))
import Relude hiding (Compose, id, many, some, state)

startKeyword ::
  DecoderState -> Text -> Text -> Int -> Either DecodeError DecoderResult
startKeyword decoder input keyword consumed =
  case keywordState keyword consumed of
    Nothing -> Left (InvalidKeyword keyword)
    Just state ->
      stepKeyword
        decoder {decoderInput = input, decoderPhase = DecoderPhaseKeyword state}
        state

-- | The state for a keyword literal; 'Nothing' for anything else.
-- The dispatcher passes literals only, so 'Nothing' is unreachable
-- from valid input — but reported, never silently matched as null.
keywordState :: Text -> Int -> Maybe KeywordState
keywordState keyword n
  | keyword == "null" = Just (KeywordNull n)
  | keyword == "true" = Just (KeywordTrue n)
  | keyword == "false" = Just (KeywordFalse n)
  | otherwise = Nothing

-- | Single table for a keyword state's literal, progress and event.
-- Inlined into 'stepKeyword' and 'finalizeKeyword'.
{-# INLINE keywordInfo #-}
keywordInfo :: KeywordState -> (Text, Int, JSONEvent)
keywordInfo (KeywordNull n) = ("null", n, JSONNull)
keywordInfo (KeywordTrue n) = ("true", n, JSONBool True)
keywordInfo (KeywordFalse n) = ("false", n, JSONBool False)

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
    (keyword, index, _) = keywordInfo state

advanceKeyword :: KeywordState -> KeywordState
advanceKeyword state = case state of
  KeywordNull n -> KeywordNull (n + 1)
  KeywordTrue n -> KeywordTrue (n + 1)
  KeywordFalse n -> KeywordFalse (n + 1)

finalizeKeyword :: DecoderState -> Either DecodeError DecoderResult
finalizeKeyword decoder = case decoderPhase decoder of
  DecoderPhaseKeyword state -> do
    let (keyword, index, event) = keywordInfo state
    if index == T.length keyword
      then emitEvent event (decoderInput decoder) decoder
      else Left $ InvalidKeyword keyword
  _ -> Left $ InvalidKeyword mempty
