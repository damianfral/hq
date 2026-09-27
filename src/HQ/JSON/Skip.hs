{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.JSON.Skip where

import Control.Monad.Error.Class (MonadError (throwError))
import Data.Char (digitToInt, isDigit, isHexDigit)
import qualified Data.Text as T
import HQ.Error (HQError (..))
import HQ.JSON.Decoder
  ( Context (..),
    DecodeError (..),
    Decoder (..),
    KeywordState (..),
    NumberPhase,
    NumberState (..),
    NumberStep (..),
    ReversedString (..),
    StreamIO,
    advanceKeyword,
    advanceNumber,
    finishValue,
    isHighSurrogate,
    isJsonDelimiter,
    isLowSurrogate,
    isValidNumberFinal,
    isWhitespace,
    keywordState,
    reversedStringToText,
    startNumberState,
  )
import HQ.JSON.Event (JSONEvent (..))
import Relude hiding (Compose, id, many, some, state)
import qualified Streaming.Prelude as S

--------------------------------------------------------------------------------
-- Text-level value skipping
--------------------------------------------------------------------------------
--
-- Skipping validates exactly like the decoder (same states, same
-- errors) but materializes nothing: no events, no string 'Text's, no
-- 'Scientific' numbers. Numbers keep a small reversed buffer solely
-- so 'InvalidNumber' payloads match the decoder's.

-- | Structural positions while skipping, mirroring the decoder states
-- that can precede a skipped region.
data SkipExpect
  = ExpectValue
  | ExpectKey
  | ExpectColon
  | ExpectObjComma
  | ExpectArrComma
  deriving (Eq, Show)

-- | Pull the next chunk when the current text is empty, skipping empty
-- chunks. Exhaustion inside a skipped region is 'UnexpectedEnd',
-- exactly like the decoder stalling on 'NeedInput' at end of input.
pullSkipText :: Text -> StreamIO Text () -> ExceptT HQError IO (Text, StreamIO Text ())
pullSkipText input text
  | T.null input = do
      result <- S.next text
      case result of
        Left () -> throwError (HQDecodeError UnexpectedEnd)
        Right (chunk, rest) -> pullSkipText chunk rest
  | otherwise = pure (input, text)

-- | The next non-whitespace character, pulling more chunks as needed.
nextSkipChar :: Text -> StreamIO Text () -> ExceptT HQError IO (Char, Text, StreamIO Text ())
nextSkipChar input text = case T.uncons (T.dropWhile isWhitespace input) of
  Just (c, rest) -> pure (c, rest, text)
  Nothing -> do
    result <- S.next text
    case result of
      Left () -> throwError (HQDecodeError UnexpectedEnd)
      Right (chunk, rest) -> nextSkipChar chunk rest

-- | Skip exactly one JSON value starting at the head of the text
-- (leading whitespace is allowed). The stack is the enclosing
-- context, used for error categories and for detecting completion:
-- the skip ends when the stack is back at its entry depth. Returns
-- the unconsumed remainder and the rest of the stream.
skipValueText :: [Context] -> Text -> StreamIO Text () -> ExceptT HQError IO (Text, StreamIO Text ())
skipValueText stack = skipExpect (length stack) stack ExpectValue

-- | Skip an object member value: the text starts where the key ended,
-- so a colon is required first (mirroring 'ParserStateObjectColon').
skipMemberValueText :: [Context] -> Text -> StreamIO Text () -> ExceptT HQError IO (Text, StreamIO Text ())
skipMemberValueText stack input text = do
  (c, rest, text') <- nextSkipChar input text
  case c of
    ':' -> skipExpect (length stack) stack ExpectValue rest text'
    _ -> throwError (HQDecodeError ExpectedColon)

-- | Skip the rest of the container whose opening event was just
-- emitted (decoder positioned after it). Pops the container and
-- advances past the value, like 'emitContainerEnd' followed by
-- 'finishValue', but validates without materializing anything.
skipContainerText ::
  JSONEvent -> Decoder -> StreamIO Text () -> ExceptT HQError IO (Decoder, StreamIO Text ())
skipContainerText open decoder text = case open of
  JSONBeginArray -> skipRest ExpectValue
  JSONBeginObject -> skipRest ExpectKey
  _ -> pure (decoder, text)
  where
    -- The opener is already consumed, so the skip ends when the stack
    -- pops back below its entry depth.
    skipRest expect = do
      let base = length (decoderStack decoder) - 1
      (remainder, rest) <- skipExpect base (decoderStack decoder) expect (decoderInput decoder) text
      case decoderStack decoder of
        _ : ctxs -> pure (finishValue decoder {decoderInput = remainder, decoderStack = ctxs}, rest)
        [] -> pure (finishValue decoder {decoderInput = remainder}, rest)

-- | Structural skip loop. A value completing when the stack is back at
-- its entry depth ends the skip and returns the remainder.
skipExpect ::
  Int -> [Context] -> SkipExpect -> Text -> StreamIO Text () -> ExceptT HQError IO (Text, StreamIO Text ())
skipExpect base stack ExpectValue input text = do
  (c, rest, text') <- nextSkipChar input text
  case c of
    '{' -> skipExpect base (ContextObject : stack) ExpectKey rest text'
    '[' -> skipExpect base (ContextArray : stack) ExpectValue rest text'
    '"' -> do
      (rest', text'') <- skipStringText rest text'
      afterValue base stack rest' text''
    't' -> skipKeywordCont "true" 1 rest text'
    'f' -> skipKeywordCont "false" 1 rest text'
    'n' -> skipKeywordCont "null" 1 rest text'
    _
      | c == '-' || isDigit c -> do
          (rest', text'') <- skipNumberText (startNumberState c) rest text'
          afterValue base stack rest' text''
      | c == ']' -> case stack of
          ContextArray : ctxs -> afterValue base ctxs rest text'
          _ -> throwError (HQDecodeError (UnexpectedChar c))
      | otherwise -> throwError (HQDecodeError (UnexpectedChar c))
  where
    skipKeywordCont keyword consumed rest' text'' = do
      (rest'', text''') <- skipKeywordText (keywordState keyword consumed) rest' text''
      afterValue base stack rest'' text'''
skipExpect base stack ExpectKey input text = do
  (c, rest, text') <- nextSkipChar input text
  case c of
    '}' -> case stack of
      _ : ctxs -> afterValue base ctxs rest text'
      [] -> throwError (HQDecodeError ExpectedObjectKey)
    '"' -> do
      (rest', text'') <- skipStringText rest text'
      skipExpect base stack ExpectColon rest' text''
    _ -> throwError (HQDecodeError ExpectedObjectKey)
skipExpect base stack ExpectColon input text = do
  (c, rest, text') <- nextSkipChar input text
  case c of
    ':' -> skipExpect base stack ExpectValue rest text'
    _ -> throwError (HQDecodeError ExpectedColon)
skipExpect base stack ExpectObjComma input text = do
  (c, rest, text') <- nextSkipChar input text
  case c of
    ',' -> skipExpect base stack ExpectKey rest text'
    '}' -> case stack of
      _ : ctxs -> afterValue base ctxs rest text'
      [] -> throwError (HQDecodeError ExpectedCommaOrEnd)
    _ -> throwError (HQDecodeError ExpectedCommaOrEnd)
skipExpect base stack ExpectArrComma input text = do
  (c, rest, text') <- nextSkipChar input text
  case c of
    ',' -> skipExpect base stack ExpectValue rest text'
    ']' -> case stack of
      _ : ctxs -> afterValue base ctxs rest text'
      [] -> throwError (HQDecodeError ExpectedCommaOrEnd)
    _ -> throwError (HQDecodeError ExpectedCommaOrEnd)

-- | A value just completed: return when back at the entry depth,
-- otherwise expect the enclosing container's separator.
afterValue ::
  Int -> [Context] -> Text -> StreamIO Text () -> ExceptT HQError IO (Text, StreamIO Text ())
afterValue base stack rest text'
  | length stack <= base = pure (rest, text')
  | otherwise = case stack of
      ContextArray : _ -> skipExpect base stack ExpectArrComma rest text'
      ContextObject : _ -> skipExpect base stack ExpectObjComma rest text'
      [] -> pure (rest, text')

-- | Skip a string starting after its opening quote. Returns the text
-- after the closing quote. Mirrors 'consumeString' without buffering.
skipStringText :: Text -> StreamIO Text () -> ExceptT HQError IO (Text, StreamIO Text ())
skipStringText = go
  where
    go :: Text -> StreamIO Text () -> ExceptT HQError IO (Text, StreamIO Text ())
    go inp txt = do
      let rest = T.dropWhile isStringChar inp
      case T.uncons rest of
        Nothing -> do
          (chunk, rest') <- pullSkipText mempty txt
          go chunk rest'
        Just (c, rest')
          | c == '"' -> pure (rest', txt)
          | c == '\\' -> skipEscape rest' txt
          | otherwise -> throwError (HQDecodeError (UnexpectedChar c))
    isStringChar c = c /= '"' && c /= '\\' && ord c >= 0x20

-- | Skip one escape sequence. Mirrors 'consumeStringEscape' without
-- buffering.
skipEscape :: Text -> StreamIO Text () -> ExceptT HQError IO (Text, StreamIO Text ())
skipEscape input text = do
  (chunk, rest) <- pullSkipText input text
  case T.uncons chunk of
    Nothing -> skipEscape mempty rest
    Just (c, rest')
      | c == '"' || c == '\\' || c == '/' -> skipStringText rest' rest
      | c == 'b' || c == 'f' || c == 'n' || c == 'r' || c == 't' -> skipStringText rest' rest
      | c == 'u' -> skipUnicode 0 0 rest' rest
      | otherwise -> throwError (HQDecodeError (InvalidEscape c))

-- | Skip a @\u@ escape's four hex digits. Mirrors 'consumeUnicode''
-- without buffering: over-long input keeps the extra digits for the
-- string scan, exhausted chunks pull more, anything else is invalid.
skipUnicode :: Int -> Int -> Text -> StreamIO Text () -> ExceptT HQError IO (Text, StreamIO Text ())
skipUnicode value digits input text = do
  (chunk, rest0) <- pullSkipText input text
  let needed = 4 - digits
      hex = T.take needed (T.takeWhile isHexDigit chunk)
      rest' = T.drop (T.length hex) chunk
      value' = T.foldl' (\v c -> v * 16 + digitToInt c) value hex
      digits' = digits + T.length hex
  if digits' >= 4
    then finishUnicodeSkip rest' rest0 value'
    else
      if T.null rest'
        then skipUnicode value' digits' mempty rest0
        else throwError (HQDecodeError InvalidUnicodeEscape)

-- | Validate a completed @\u@ escape. Mirrors 'finishUnicode' without
-- buffering.
finishUnicodeSkip :: Text -> StreamIO Text () -> Int -> ExceptT HQError IO (Text, StreamIO Text ())
finishUnicodeSkip input text value
  | isHighSurrogate value = skipLowSurrogate input text value
  | isLowSurrogate value = throwError (HQDecodeError InvalidSurrogatePair)
  | otherwise = skipStringText input text

-- | Skip a low-surrogate escape after a high surrogate. Mirrors
-- 'consumeLowSurrogate' without buffering.
skipLowSurrogate :: Text -> StreamIO Text () -> Int -> ExceptT HQError IO (Text, StreamIO Text ())
skipLowSurrogate input text high = do
  (chunk, rest) <- pullSkipText input text
  case T.uncons chunk of
    Nothing -> skipLowSurrogate mempty rest high
    Just ('\\', rest') -> case T.uncons rest' of
      Nothing -> skipLowSurrogateBackslash rest high
      Just ('u', rest'') -> skipLowDigits rest'' rest high 0 0
      _ -> throwError (HQDecodeError InvalidSurrogatePair)
    _ -> throwError (HQDecodeError InvalidSurrogatePair)

-- | The low surrogate's backslash arrived at a chunk end; the next
-- chunk must start with @u@. Mirrors the 'AfterHighSurrogate' resume.
skipLowSurrogateBackslash :: StreamIO Text () -> Int -> ExceptT HQError IO (Text, StreamIO Text ())
skipLowSurrogateBackslash text high = do
  (chunk, rest) <- pullSkipText mempty text
  case T.uncons chunk of
    Nothing -> skipLowSurrogateBackslash rest high
    Just ('u', rest') -> skipLowDigits rest' rest high 0 0
    _ -> throwError (HQDecodeError InvalidSurrogatePair)

-- | Skip the low surrogate's four hex digits. Mirrors
-- 'consumeLowSurrogateDigits' without buffering, including forgetting
-- a pending high surrogate when digits stall at a chunk end.
skipLowDigits ::
  Text -> StreamIO Text () -> Int -> Int -> Int -> ExceptT HQError IO (Text, StreamIO Text ())
skipLowDigits input text high value digits
  | digits == 4 =
      if isLowSurrogate value
        then skipStringText input text
        else throwError (HQDecodeError InvalidSurrogatePair)
  | otherwise = do
      (chunk, rest) <- pullSkipText input text
      case T.uncons chunk of
        Nothing -> skipLowDigits mempty rest high value digits
        Just _ -> do
          let remaining = 4 - digits
              limited = T.take remaining chunk
              (hexDigits, _) = T.span isHexDigit limited
              consumed = T.length hexDigits
              value' = T.foldl' (\ac c -> ac * 16 + digitToInt c) value hexDigits
              digits' = digits + consumed
              rest' = T.drop consumed chunk
          if digits' == 4
            then
              if isLowSurrogate value'
                then skipStringText rest' rest
                else throwError (HQDecodeError InvalidSurrogatePair)
            else
              -- The chunk is exhausted mid-escape: keep the pending
              -- high surrogate and continue with more input, mirroring
              -- how feeding appends chunks before stepping.
              if T.null rest'
                then skipLowDigits mempty rest high value' digits'
                else throwError (HQDecodeError InvalidUnicodeEscape)

-- | Skip a number from its saved state. Mirrors 'stepNumber' without
-- building a 'Scientific': phases advance identically and error
-- payloads match, but digits only accumulate into a message buffer.
skipNumberText :: NumberState -> Text -> StreamIO Text () -> ExceptT HQError IO (Text, StreamIO Text ())
skipNumberText numState =
  go (numberBuffer numState) (numberPhase numState)
  where
    go :: ReversedString -> NumberPhase -> Text -> StreamIO Text () -> ExceptT HQError IO (Text, StreamIO Text ())
    go (ReversedString rev) phase inp txt
      | T.null inp = do
          result <- S.next txt
          case result of
            Left ()
              | isValidNumberFinal phase -> pure (mempty, txt)
              | otherwise -> throwError (HQDecodeError (InvalidNumber (reversedStringToText (ReversedString rev))))
            Right (chunk, rest) -> go (ReversedString rev) phase chunk rest
      | otherwise = loop rev phase 0
      where
        len = T.length inp
        loop !r !p !pos
          | pos >= len = go (ReversedString r) p mempty txt
          | otherwise =
              let c = T.index inp pos
               in case advanceNumber p c of
                    NumberEnd
                      | isValidNumberFinal p -> pure (T.drop pos inp, txt)
                      | otherwise -> throwError (HQDecodeError (InvalidNumber (reversedStringToText (ReversedString r))))
                    NumberError -> throwError (HQDecodeError (InvalidNumber (reversedStringToText (ReversedString r) <> one c)))
                    NumberStep p' -> loop (c : r) p' (pos + 1)

-- | Skip a keyword from its saved state. Mirrors 'stepKeyword',
-- including its end-of-input rules: a complete keyword at
-- exhaustion succeeds, an incomplete one is 'InvalidKeyword'.
skipKeywordText :: KeywordState -> Text -> StreamIO Text () -> ExceptT HQError IO (Text, StreamIO Text ())
skipKeywordText state input text =
  let (keyword, index) = case state of
        KeywordNull i -> ("null" :: Text, i)
        KeywordTrue i -> ("true" :: Text, i)
        KeywordFalse i -> ("false" :: Text, i)
   in if index == T.length keyword
        then finalCheck keyword input text
        else matchLoop keyword index input text
  where
    finalCheck :: Text -> Text -> StreamIO Text () -> ExceptT HQError IO (Text, StreamIO Text ())
    finalCheck keyword inp txt
      | T.null inp = do
          result <- S.next txt
          case result of
            Left () -> pure (mempty, txt)
            Right (chunk, rest) -> finalCheck keyword chunk rest
      | otherwise = case T.uncons inp of
          Nothing -> finalCheck keyword mempty txt
          Just (c, _)
            | isJsonDelimiter c -> pure (inp, txt)
            | otherwise -> throwError (HQDecodeError (InvalidKeyword keyword))
    matchLoop :: Text -> Int -> Text -> StreamIO Text () -> ExceptT HQError IO (Text, StreamIO Text ())
    matchLoop keyword index inp txt
      | T.null inp = do
          result <- S.next txt
          case result of
            Left () -> throwError (HQDecodeError (InvalidKeyword keyword))
            Right (chunk, rest) -> matchLoop keyword index chunk rest
      | otherwise = case T.uncons inp of
          Nothing -> matchLoop keyword index mempty txt
          Just (c, rest)
            | c == T.index keyword index ->
                skipKeywordText (advanceKeyword state) rest txt
            | otherwise -> throwError (HQDecodeError (InvalidKeyword keyword))
