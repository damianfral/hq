{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | String parsing for the streaming JSON decoder, including escapes,
-- unicode escapes and surrogate pairs.
module HQ.JSON.Decoder.String where

import Data.Bits (Bits (..), shiftL)
import Data.Char (digitToInt, isHexDigit)
import qualified Data.Text as T
import HQ.JSON.Decoder.Core
import HQ.JSON.Decoder.Error (DecodeError (..))
import HQ.JSON.Decoder.StringBuffer
import HQ.JSON.Event (JSONEvent (..))
import Relude hiding (Compose, id, many, some, state)

startString :: StringTarget -> Text -> DecoderState -> Either DecodeError DecoderResult
startString target input = consumeString target input emptyStringBuffer

consumeString ::
  StringTarget -> Text -> StringBuffer -> DecoderState -> Either DecodeError DecoderResult
consumeString target input buffer decoder = case T.uncons rest of
  Nothing ->
    let bufferedTarget = BufferedStringTarget target newBuffer
        parserString = DecoderPhaseString (InString bufferedTarget)
        decoder' = decoder {decoderInput = mempty, decoderPhase = parserString}
     in Right $ NeedInput decoder'
  Just (c, rest')
    | c == '"' -> case buffer of
        -- Single escape-free fragment (the common case for keys and
        -- short values): reference the input slice directly instead
        -- of copying it through 'StringBuffer' and 'T.concat'.
        StringBuffer [] -> finishStringText target chunk rest' decoder
        _ -> finishString target newBuffer rest' decoder
    | c == '\\' -> consumeStringEscape target rest' newBuffer decoder
    | otherwise -> Left (UnexpectedChar c)
  where
    (chunk, rest) = T.span (\c -> c /= '"' && c /= '\\' && ord c >= 0x20) input
    !newBuffer = appendStringBuffer chunk buffer

consumeStringEscape ::
  StringTarget -> Text -> StringBuffer -> DecoderState -> Either DecodeError DecoderResult
consumeStringEscape target input buffer decoder = case T.uncons input of
  Nothing ->
    let parserString =
          DecoderPhaseString $ AfterEscape $ BufferedStringTarget target buffer
     in Right $ NeedInput decoder {decoderInput = mempty, decoderPhase = parserString}
  Just (c, rest) -> case c of
    '"' -> consumeString target rest (appendCharStringBuffer '"' buffer) decoder
    '\\' -> consumeString target rest (appendCharStringBuffer '\\' buffer) decoder
    '/' -> consumeString target rest (appendCharStringBuffer '/' buffer) decoder
    'b' -> consumeString target rest (appendCharStringBuffer '\b' buffer) decoder
    'f' -> consumeString target rest (appendCharStringBuffer '\f' buffer) decoder
    'n' -> consumeString target rest (appendCharStringBuffer '\n' buffer) decoder
    'r' -> consumeString target rest (appendCharStringBuffer '\r' buffer) decoder
    't' -> consumeString target rest (appendCharStringBuffer '\t' buffer) decoder
    'u' -> consumeUnicode target rest buffer decoder
    _ -> Left (InvalidEscape c)

consumeUnicode ::
  StringTarget -> Text -> StringBuffer -> DecoderState -> Either DecodeError DecoderResult
consumeUnicode target input buffer = consumeUnicode' target input buffer 0 0

consumeUnicode' ::
  StringTarget ->
  Text ->
  StringBuffer ->
  Int ->
  Int ->
  DecoderState ->
  Either DecodeError DecoderResult
consumeUnicode' target input buffer value digits decoder
  | newDigits >= 4 = finishUnicode target rest buffer newValue decoder
  | T.null rest =
      let unicode = Unicode newValue newDigits
          bufferedString = BufferedStringTarget target buffer
          parserString = DecoderPhaseString $ InUnicodeEscape bufferedString unicode
          newDecoder = decoder {decoderInput = "", decoderPhase = parserString}
       in Right $ NeedInput newDecoder
  | otherwise = Left InvalidUnicodeEscape
  where
    needed = 4 - digits
    -- Take at most the digits still needed to complete the escape: a
    -- further hex digit belongs to the text following the escape.
    hex = T.take needed (T.takeWhile isHexDigit input)
    rest = T.drop (T.length hex) input
    newValue = T.foldl' (\v c -> v * 16 + digitToInt c) value hex
    newDigits = digits + T.length hex

finishUnicode ::
  StringTarget ->
  Text ->
  StringBuffer ->
  Int ->
  DecoderState ->
  Either DecodeError DecoderResult
finishUnicode target input buffer value decoder
  | isHighSurrogate value =
      let bufferedStringT = BufferedStringTarget target buffer
          parserString = DecoderPhaseString $ AfterHighSurrogate bufferedStringT value
          newDecoder = decoder {decoderInput = input, decoderPhase = parserString}
       in Right $ NeedInput newDecoder
  | isLowSurrogate value = Left InvalidSurrogatePair
  | otherwise =
      consumeString target input (appendCharStringBuffer (chr value) buffer) decoder

stepString :: DecoderState -> StringState -> Either DecodeError DecoderResult
stepString decoder state =
  case state of
    InString (BufferedStringTarget target buffer) ->
      consumeString target (decoderInput decoder) buffer decoder
    AfterEscape (BufferedStringTarget target buffer) ->
      consumeStringEscape target (decoderInput decoder) buffer decoder
    InUnicodeEscape (BufferedStringTarget target buffer) (Unicode value digits) ->
      consumeUnicode' target (decoderInput decoder) buffer value digits decoder
    AfterHighSurrogate (BufferedStringTarget target buffer) high ->
      consumeLowSurrogate target (decoderInput decoder) buffer high decoder
    AfterLowBackslash (BufferedStringTarget target buffer) high ->
      consumeLowBackslash target (decoderInput decoder) buffer high decoder
    InLowSurrogateEscape (BufferedStringTarget target buffer) high (Unicode value digits) ->
      consumeLowSurrogateDigits target (decoderInput decoder) buffer high value digits decoder

consumeLowSurrogate ::
  StringTarget ->
  Text ->
  StringBuffer ->
  Int ->
  DecoderState ->
  Either DecodeError DecoderResult
consumeLowSurrogate target input buffer high decoder = case T.uncons input of
  Nothing -> Right $ NeedInput decoder {decoderInput = mempty}
  Just ('\\', rest) -> case T.uncons rest of
    Nothing ->
      let parserString = DecoderPhaseString $ AfterLowBackslash (BufferedStringTarget target buffer) high
       in Right $ NeedInput decoder {decoderInput = mempty, decoderPhase = parserString}
    Just ('u', rest') ->
      consumeLowSurrogateDigits target rest' buffer high 0 0 decoder
    _ -> Left InvalidSurrogatePair
  _ -> Left InvalidSurrogatePair

-- | Resume after a chunk split between the low escape's backslash and
-- @u@: only @u@ may follow, continuing into the low hex digits.
consumeLowBackslash ::
  StringTarget ->
  Text ->
  StringBuffer ->
  Int ->
  DecoderState ->
  Either DecodeError DecoderResult
consumeLowBackslash target input buffer high decoder = case T.uncons input of
  Nothing -> Right $ NeedInput decoder {decoderInput = mempty}
  Just ('u', rest) ->
    consumeLowSurrogateDigits target rest buffer high 0 0 decoder
  _ -> Left InvalidSurrogatePair

consumeLowSurrogateDigits ::
  StringTarget ->
  Text ->
  StringBuffer ->
  Int ->
  Int ->
  Int ->
  DecoderState ->
  Either DecodeError DecoderResult
consumeLowSurrogateDigits target input buffer high value digits decoder
  | digits == 4 =
      if isLowSurrogate value
        then
          let codepoint =
                0x10000
                  + ((high - 0xD800) `shiftL` 10)
                  + (value - 0xDC00)
              newBuffer = appendCharStringBuffer (chr codepoint) buffer
           in consumeString target input newBuffer decoder
        else Left InvalidSurrogatePair
  | otherwise =
      let remaining = 4 - digits
          limitedInput = T.take remaining input
          (hexDigits, _) = T.span isHexDigit limitedInput
          consumed = T.length hexDigits
          newValue = T.foldl' (\ac c -> ac * 16 + digitToInt c) value hexDigits
          newDigits = digits + consumed
          rest = T.drop consumed input
       in if newDigits == 4
            then
              if isLowSurrogate newValue
                then
                  let codepoint =
                        0x10000
                          + ((high - 0xD800) `shiftL` 10)
                          + (newValue - 0xDC00)
                      newBuffer = appendCharStringBuffer (chr codepoint) buffer
                   in consumeString target rest newBuffer decoder
                else Left InvalidSurrogatePair
            else
              if T.null rest
                then
                  Right
                    $ NeedInput
                      decoder
                        { decoderInput = mempty,
                          decoderPhase =
                            DecoderPhaseString
                              $ InLowSurrogateEscape
                                (BufferedStringTarget target buffer)
                                high
                                (Unicode newValue newDigits)
                        }
                else
                  Left InvalidUnicodeEscape

finishString ::
  StringTarget ->
  StringBuffer ->
  Text ->
  DecoderState ->
  Either DecodeError DecoderResult
finishString target = finishStringText target . finishStringBuffer

-- | Finish a string from its materialized text. 'finishString' funnels
-- here after concatenating multi-fragment buffers, while the
-- single-fragment fast path in 'consumeString' calls it directly with
-- the input slice.
finishStringText ::
  StringTarget ->
  Text ->
  Text ->
  DecoderState ->
  Either DecodeError DecoderResult
finishStringText target text remaining decoder = case target of
  StringValue ->
    emitScalar (JSONString text) remaining decoder
  StringKey -> do
    let key = JSONObjectKey text
    let newDecoder =
          decoder
            { decoderInput = remaining,
              decoderPhase = DecoderPhaseObjectColon
            }
    Right $ Emit key newDecoder
