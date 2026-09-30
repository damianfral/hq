{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.JSON.Skip where

import Control.Monad.Error.Class (MonadError (throwError))
import Data.ByteString.Builder (Builder, char7, charUtf8)
import Data.Char (digitToInt, isDigit, isHexDigit)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8Builder)
import HQ.JSON.Decoder
import HQ.JSON.Decoder.Keyword (advanceKeyword, keywordState)
import HQ.JSON.Decoder.Number
import HQ.JSON.Depth (NestDepth (..), deeper, shallower)
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
-- so 'InvalidNumber' payloads match the decoder's. Every @skip*@ and
-- @collect*@ function below mirrors its decoder twin; only differences
-- from the twin are documented.

-- | Structural positions while skipping.
data SkipExpect
  = ExpectValue
  | ExpectKey
  | ExpectColon
  | ExpectObjComma
  | ExpectArrComma
  deriving (Eq, Show)

-- | Keyword literals as shared CAFs (fresh literals repack per keyword).
trueKeyword, falseKeyword, nullKeyword :: Text
trueKeyword = "true"
falseKeyword = "false"
nullKeyword = "null"

-- | Match a keyword remainder with total 'T.uncons' steps (no
-- 'HasCallStack' costs, unlike 'T.stripPrefix').
matchKeyword :: Text -> Text -> Maybe Text
matchKeyword keyword inp
  | keyword == trueKeyword = match3 'r' 'u' 'e' inp
  | keyword == falseKeyword = match4 'a' 'l' 's' 'e' inp
  | otherwise = match3 'u' 'l' 'l' inp

match3 :: Char -> Char -> Char -> Text -> Maybe Text
match3 c1 c2 c3 t = case T.uncons t of
  Just (d1, r1) | d1 == c1 -> case T.uncons r1 of
    Just (d2, r2) | d2 == c2 -> case T.uncons r2 of
      Just (d3, r3) | d3 == c3 -> Just r3
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing

-- | Match four literal characters, returning the text after them.
match4 :: Char -> Char -> Char -> Char -> Text -> Maybe Text
match4 c1 c2 c3 c4 t = case T.uncons t of
  Just (d1, r1) | d1 == c1 -> case T.uncons r1 of
    Just (d2, r2) | d2 == c2 -> case T.uncons r2 of
      Just (d3, r3) | d3 == c3 -> case T.uncons r3 of
        Just (d4, r4) | d4 == c4 -> Just r4
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing

-- | Pull the next chunk, skipping empties. Exhaustion here is
-- 'UnexpectedEnd'.
pullSkipText ::
  Text -> StreamIO Text () -> ExceptT HQError IO (Text, StreamIO Text ())
pullSkipText !input !text
  | T.null input = do
      result <- S.next text
      case result of
        Left () -> throwError (HQDecodeError UnexpectedEnd)
        Right (chunk, rest) -> pullSkipText chunk rest
  | otherwise = pure (input, text)

-- | The next non-whitespace character, pulling more chunks as needed.
nextSkipChar ::
  Text -> StreamIO Text () -> ExceptT HQError IO (Char, Text, StreamIO Text ())
nextSkipChar input text = case T.uncons (T.dropWhile isWhitespace input) of
  Just (c, rest) -> pure (c, rest, text)
  Nothing -> do
    result <- S.next text
    case result of
      Left () -> throwError (HQDecodeError UnexpectedEnd)
      Right (chunk, rest) -> nextSkipChar chunk rest

-- | Skip an object member value; a colon is required first.
skipMemberValueText ::
  NestDepth ->
  [DecodeContext] ->
  Text ->
  StreamIO Text () ->
  ExceptT HQError IO (Text, StreamIO Text ())
skipMemberValueText !depth !stack !input !text = do
  (c, rest, text') <- nextSkipChar input text
  case c of
    ':' -> skipExpect depth depth stack ExpectValue rest text'
    _ -> throwError (HQDecodeError ExpectedColon)

-- | Skip the rest of the container whose opening event was just emitted.
skipContainerText ::
  JSONEvent ->
  DecoderState ->
  StreamIO Text () ->
  ExceptT HQError IO (DecoderState, StreamIO Text ())
skipContainerText open decoder@DecoderState {..} text = case open of
  JSONBeginArray -> skipRest ExpectValue
  JSONBeginObject -> skipRest ExpectKey
  _ -> pure (decoder, text)
  where
    -- The opener is already consumed, so the skip ends when the depth
    -- pops back below its entry level.
    skipRest expect = do
      let depth = decoderNestDepth
          base = shallower depth
      (remainder, rest) <-
        skipExpect base depth decoderStack expect decoderInput text
      case decoderStack of
        _ : ctxs ->
          let newDecoder = decoder {decoderInput = remainder, decoderStack = ctxs}
           in pure (finishValue newDecoder, rest)
        [] -> pure (finishValue decoder {decoderInput = remainder}, rest)

-- | Structural skip loop; ends when the depth pops back to its entry level.
skipExpect ::
  NestDepth ->
  NestDepth ->
  [DecodeContext] ->
  SkipExpect ->
  Text ->
  StreamIO Text () ->
  ExceptT HQError IO (Text, StreamIO Text ())
skipExpect base depth stack ExpectValue input text = do
  (c, rest, text') <- nextSkipChar input text
  case c of
    '{' -> skipExpect base (deeper depth) (DecodeObject : stack) ExpectKey rest text'
    '[' -> skipExpect base (deeper depth) (DecodeArray : stack) ExpectValue rest text'
    '"' -> do
      (rest', text'') <- skipStringText rest text'
      afterValue base depth stack rest' text''
    't' -> skipKeywordFast trueKeyword 1 rest text'
    'f' -> skipKeywordFast falseKeyword 1 rest text'
    'n' -> skipKeywordFast nullKeyword 1 rest text'
    _
      | c == '-' || isDigit c -> do
          (rest', text'') <- skipNumberText (startNumberState c) rest text'
          afterValue base depth stack rest' text''
      | c == ']' -> case stack of
          DecodeArray : ctxs -> afterValue base (shallower depth) ctxs rest text'
          _ -> throwError (HQDecodeError (UnexpectedChar c))
      | otherwise -> throwError (HQDecodeError (UnexpectedChar c))
  where
    -- \| Fast path for @true@/@false@/@null@ via one prefix check;
    -- split literals fall back with identical errors.
    skipKeywordFast ::
      Text ->
      Int ->
      Text ->
      StreamIO Text () ->
      ExceptT HQError IO (Text, StreamIO Text ())
    skipKeywordFast keyword consumed inp txt =
      case matchKeyword keyword inp of
        Just after -> case T.uncons after of
          Just (c, _)
            | isJsonDelimiter c -> afterValue base depth stack after txt
          _ -> skipKeywordSlow keyword consumed base depth stack inp txt
        Nothing -> skipKeywordSlow keyword consumed base depth stack inp txt
skipExpect base depth stack ExpectKey input text = do
  (c, rest, text') <- nextSkipChar input text
  case c of
    '}' -> case stack of
      _ : ctxs -> afterValue base (shallower depth) ctxs rest text'
      [] -> throwError (HQDecodeError ExpectedObjectKey)
    '"' -> do
      (rest', text'') <- skipStringText rest text'
      skipExpect base depth stack ExpectColon rest' text''
    _ -> throwError (HQDecodeError ExpectedObjectKey)
skipExpect base depth stack ExpectColon input text = do
  (c, rest, text') <- nextSkipChar input text
  case c of
    ':' -> skipExpect base depth stack ExpectValue rest text'
    _ -> throwError (HQDecodeError ExpectedColon)
skipExpect base depth stack ExpectObjComma input text = do
  (c, rest, text') <- nextSkipChar input text
  case c of
    ',' -> skipExpect base depth stack ExpectKey rest text'
    '}' -> case stack of
      _ : ctxs -> afterValue base (shallower depth) ctxs rest text'
      [] -> throwError (HQDecodeError ExpectedCommaOrEnd)
    _ -> throwError (HQDecodeError ExpectedCommaOrEnd)
skipExpect base depth stack ExpectArrComma input text = do
  (c, rest, text') <- nextSkipChar input text
  case c of
    ',' -> skipExpect base depth stack ExpectValue rest text'
    ']' -> case stack of
      _ : ctxs -> afterValue base (shallower depth) ctxs rest text'
      [] -> throwError (HQDecodeError ExpectedCommaOrEnd)
    _ -> throwError (HQDecodeError ExpectedCommaOrEnd)

-- | Fallback for split/invalid keywords; top-level to avoid a
-- per-keyword closure.
skipKeywordSlow ::
  Text ->
  Int ->
  NestDepth ->
  NestDepth ->
  [DecodeContext] ->
  Text ->
  StreamIO Text () ->
  ExceptT HQError IO (Text, StreamIO Text ())
skipKeywordSlow keyword consumed base depth stack inp txt = do
  (rest', txt') <-
    skipKeywordText (keywordState keyword consumed) inp txt
  afterValue base depth stack rest' txt'

-- | A value just completed: return at entry depth, else expect the
-- enclosing separator.
afterValue ::
  NestDepth ->
  NestDepth ->
  [DecodeContext] ->
  Text ->
  StreamIO Text () ->
  ExceptT HQError IO (Text, StreamIO Text ())
afterValue base depth stack rest text'
  | depth <= base = pure (rest, text')
  | otherwise = case stack of
      DecodeArray : _ -> skipExpect base depth stack ExpectArrComma rest text'
      DecodeObject : _ -> skipExpect base depth stack ExpectObjComma rest text'
      [] -> pure (rest, text')

-- | Verbatim string bytes; shared so skip and collect split identically.
isStringChar :: Char -> Bool
isStringChar c = c /= '"' && c /= '\\' && ord c >= 0x20

-- | Skip a string starting after its opening quote.
skipStringText ::
  Text -> StreamIO Text () -> ExceptT HQError IO (Text, StreamIO Text ())
skipStringText inp txt = do
  let rest = T.dropWhile isStringChar inp
  case T.uncons rest of
    Nothing -> do
      (chunk, rest') <- pullSkipText mempty txt
      skipStringText chunk rest'
    Just (c, rest')
      | c == '"' -> pure (rest', txt)
      | c == '\\' -> skipEscape rest' txt
      | otherwise -> throwError (HQDecodeError (UnexpectedChar c))

-- | Skip one escape sequence.
skipEscape ::
  Text -> StreamIO Text () -> ExceptT HQError IO (Text, StreamIO Text ())
skipEscape input text = do
  (chunk, rest) <- pullSkipText input text
  case T.uncons chunk of
    Nothing -> skipEscape mempty rest
    Just (c, rest')
      | c == '"' || c == '\\' || c == '/' -> skipStringText rest' rest
      | c == 'b' || c == 'f' || c == 'n' || c == 'r' || c == 't' ->
          skipStringText rest' rest
      | c == 'u' -> skipUnicode 0 0 rest' rest
      | otherwise -> throwError (HQDecodeError (InvalidEscape c))

-- | Skip a @\u@ escape's four hex digits.
skipUnicode ::
  Int ->
  Int ->
  Text ->
  StreamIO Text () ->
  ExceptT HQError IO (Text, StreamIO Text ())
skipUnicode !value !digits !input !text = do
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
finishUnicodeSkip ::
  Text -> StreamIO Text () -> Int -> ExceptT HQError IO (Text, StreamIO Text ())
finishUnicodeSkip !input !text !value
  | isHighSurrogate value = skipLowSurrogate input text value
  | isLowSurrogate value = throwError (HQDecodeError InvalidSurrogatePair)
  | otherwise = skipStringText input text

-- | Skip a low-surrogate escape after a high surrogate. Mirrors
-- 'consumeLowSurrogate' without buffering.
skipLowSurrogate ::
  Text -> StreamIO Text () -> Int -> ExceptT HQError IO (Text, StreamIO Text ())
skipLowSurrogate !input !text !high = do
  (chunk, rest) <- pullSkipText input text
  case T.uncons chunk of
    Nothing -> skipLowSurrogate mempty rest high
    Just ('\\', rest') -> case T.uncons rest' of
      Nothing -> skipLowSurrogateBackslash rest high
      Just ('u', rest'') -> skipLowDigits rest'' rest high 0 0
      _ -> throwError (HQDecodeError InvalidSurrogatePair)
    _ -> throwError (HQDecodeError InvalidSurrogatePair)

-- | A low surrogate's backslash ended the chunk; the next one starts with @u@.
skipLowSurrogateBackslash ::
  StreamIO Text () -> Int -> ExceptT HQError IO (Text, StreamIO Text ())
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
  Text ->
  StreamIO Text () ->
  Int ->
  Int ->
  Int ->
  ExceptT HQError IO (Text, StreamIO Text ())
skipLowDigits !input !text !high !value !digits
  | digits == 4 =
      if isLowSurrogate value
        then skipStringText input text
        else throwError $ HQDecodeError InvalidSurrogatePair
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
                else throwError $ HQDecodeError InvalidSurrogatePair
            else
              -- The chunk is exhausted mid-escape: keep the pending
              -- high surrogate and continue with more input, mirroring
              -- how feeding appends chunks before stepping.
              if T.null rest'
                then skipLowDigits mempty rest high value' digits'
                else throwError $ HQDecodeError InvalidUnicodeEscape

-- | Skip a number; digits accumulate only for error payloads.
skipNumberText ::
  NumberState ->
  Text ->
  StreamIO Text () ->
  ExceptT HQError IO (Text, StreamIO Text ())
skipNumberText numState =
  go (numberBuffer numState) (numberPhase numState)
  where
    go ::
      ReversedString ->
      NumberPhase ->
      Text ->
      StreamIO Text () ->
      ExceptT HQError IO (Text, StreamIO Text ())
    go (ReversedString rev) !phase !inp !txt
      | T.null inp = do
          result <- S.next txt
          case result of
            Left ()
              | isValidNumberFinal phase -> pure (mempty, txt)
              | otherwise ->
                  throwError
                    $ HQDecodeError
                    $ InvalidNumber (reversedStringToText (ReversedString rev))
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
                    NumberError ->
                      throwError
                        $ HQDecodeError
                        $ InvalidNumber
                        $ reversedStringToText (ReversedString r)
                        <> one c
                    NumberStep p' -> loop (c : r) p' (pos + 1)

-- | Skip a number, capturing its raw bytes.
skipNumberCollect ::
  Text ->
  StreamIO Text () ->
  ExceptT HQError IO (Text, Builder, Int, StreamIO Text ())
skipNumberCollect input text = case T.uncons input of
  Nothing -> do
    (chunk, rest) <- pullSkipText mempty text
    skipNumberCollect chunk rest
  Just (c, rest) ->
    let phase = fromMaybe NumberSign (numberPhaseFromFirstChar c)
     in collectNumber [one c] 1 phase rest text

collectNumber ::
  [Text] ->
  Int ->
  NumberPhase ->
  Text ->
  StreamIO Text () ->
  ExceptT HQError IO (Text, Builder, Int, StreamIO Text ())
collectNumber frags size phase inp txt
  | T.null inp = do
      result <- S.next txt
      case result of
        Left ()
          | isValidNumberFinal phase -> pure (mempty, build frags, size, txt)
          | otherwise ->
              throwError $ HQDecodeError $ InvalidNumber (T.concat frags)
        Right (chunk, rest) -> collectNumber frags size phase chunk rest
  | otherwise = loop frags size phase 0
  where
    len = T.length inp
    loop f s p pos
      | pos >= len = collectNumber (f <> [inp]) (s + len) p mempty txt
      | otherwise =
          let c = T.index inp pos
           in case advanceNumber p c of
                NumberEnd
                  | isValidNumberFinal p ->
                      let frags' = f <> [T.take pos inp]
                       in pure (T.drop pos inp, build frags', s + pos, txt)
                  | otherwise ->
                      throwError
                        $ HQDecodeError
                        $ InvalidNumber (T.concat (f <> [T.take pos inp]))
                NumberError ->
                  throwError
                    $ HQDecodeError
                    $ InvalidNumber (T.concat (f <> [T.take pos inp]) <> one c)
                NumberStep p' -> loop f s p' (pos + 1)
    build = foldMap encodeUtf8Builder

-- | Skip a keyword; a complete one at exhaustion succeeds.
skipKeywordText ::
  KeywordState ->
  Text ->
  StreamIO Text () ->
  ExceptT HQError IO (Text, StreamIO Text ())
skipKeywordText state input text =
  let (keyword, index) = case state of
        KeywordNull i -> ("null" :: Text, i)
        KeywordTrue i -> ("true" :: Text, i)
        KeywordFalse i -> ("false" :: Text, i)
   in if index == T.length keyword
        then finalCheck keyword input text
        else matchLoop keyword index input text
  where
    finalCheck ::
      Text ->
      Text ->
      StreamIO Text () ->
      ExceptT HQError IO (Text, StreamIO Text ())
    finalCheck !keyword !inp !txt
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
    matchLoop ::
      Text ->
      Int ->
      Text ->
      StreamIO Text () ->
      ExceptT HQError IO (Text, StreamIO Text ())
    matchLoop !keyword !index !inp !txt
      | T.null inp = do
          result <- S.next txt
          case result of
            Left () -> throwError $ HQDecodeError $ InvalidKeyword keyword
            Right (chunk, rest) -> matchLoop keyword index chunk rest
      | otherwise = case T.uncons inp of
          Nothing -> matchLoop keyword index mempty txt
          Just (c, rest)
            | c == T.index keyword index ->
                skipKeywordText (advanceKeyword state) rest txt
            | otherwise -> throwError $ HQDecodeError $ InvalidKeyword keyword

--------------------------------------------------------------------------------
-- Raw string capture
--------------------------------------------------------------------------------
--
-- Like skipping, but accumulating the consumed bytes so rewrite
-- passthrough can re-emit strings without decoding them. Escapes stay
-- verbatim, so bytes may differ from canonical re-encoding while
-- decoding to identical events.

-- | Skip a string, capturing its raw bytes (quotes excluded).
skipStringCollect ::
  Text ->
  StreamIO Text () ->
  ExceptT HQError IO (Text, Builder, Int, StreamIO Text ())
skipStringCollect = collectStringGo mempty 0

collectStringGo ::
  Builder ->
  Int ->
  Text ->
  StreamIO Text () ->
  ExceptT HQError IO (Text, Builder, Int, StreamIO Text ())
collectStringGo !accB !accS !inp !txt = do
  let (safe, rest) = T.span isStringChar inp
      accB' = accB <> encodeUtf8Builder safe
      accS' = accS + T.length safe
  case T.uncons rest of
    Nothing -> do
      (chunk, rest') <- pullSkipText mempty txt
      collectStringGo accB' accS' chunk rest'
    Just (c, rest')
      | c == '"' -> pure (rest', accB', accS', txt)
      | c == '\\' -> collectEscape accB' accS' rest' txt
      | otherwise -> throwError (HQDecodeError (UnexpectedChar c))

-- | Capture one escape sequence, appending its raw bytes.
collectEscape ::
  Builder ->
  Int ->
  Text ->
  StreamIO Text () ->
  ExceptT HQError IO (Text, Builder, Int, StreamIO Text ())
collectEscape !accB !accS !input !text = do
  (chunk, rest) <- pullSkipText input text
  case T.uncons chunk of
    Nothing -> collectEscape accB accS mempty rest
    Just (c, rest')
      | c == '"' || c == '\\' || c == '/' -> collectString (esc c) rest' rest
      | c == 'b' || c == 'f' || c == 'n' || c == 'r' || c == 't' ->
          collectString (esc c) rest' rest
      | c == 'u' -> collectUnicode (esc 'u') rest' rest 0 0
      | otherwise -> throwError (HQDecodeError (InvalidEscape c))
  where
    esc c = (accB <> char7 '\\' <> charUtf8 c, accS + 2)
    collectString (b, s) = collectStringGo b s

-- | Capture a @\u@ escape's four hex digits. Mirrors 'skipUnicode'.
collectUnicode ::
  (Builder, Int) ->
  Text ->
  StreamIO Text () ->
  Int ->
  Int ->
  ExceptT HQError IO (Text, Builder, Int, StreamIO Text ())
collectUnicode (!accB, !accS) !input !text !value !digits = do
  (chunk, rest0) <- pullSkipText input text
  let needed = 4 - digits
      hex = T.take needed (T.takeWhile isHexDigit chunk)
      rest' = T.drop (T.length hex) chunk
      value' = T.foldl' (\v c -> v * 16 + digitToInt c) value hex
      digits' = digits + T.length hex
      accB' = accB <> encodeUtf8Builder hex
      accS' = accS + T.length hex
  if digits' >= 4
    then finishUnicodeCollect accB' accS' rest' rest0 value'
    else
      if T.null rest'
        then collectUnicode (accB', accS') mempty rest0 value' digits'
        else throwError (HQDecodeError InvalidUnicodeEscape)

-- | Validate a completed @\u@ escape. Mirrors 'finishUnicodeSkip'.
finishUnicodeCollect ::
  Builder ->
  Int ->
  Text ->
  StreamIO Text () ->
  Int ->
  ExceptT HQError IO (Text, Builder, Int, StreamIO Text ())
finishUnicodeCollect !accB !accS !input !text !value
  | isHighSurrogate value = collectLowSurrogate accB accS input text value
  | isLowSurrogate value = throwError (HQDecodeError InvalidSurrogatePair)
  | otherwise = collectStringGo accB accS input text

-- | Capture a low-surrogate escape after a high surrogate. Mirrors
-- 'skipLowSurrogate'.
collectLowSurrogate ::
  Builder ->
  Int ->
  Text ->
  StreamIO Text () ->
  Int ->
  ExceptT HQError IO (Text, Builder, Int, StreamIO Text ())
collectLowSurrogate !accB !accS !input !text !high = do
  (chunk, rest) <- pullSkipText input text
  case T.uncons chunk of
    Nothing -> collectLowSurrogate accB accS mempty rest high
    Just ('\\', rest') -> case T.uncons rest' of
      Nothing -> collectLowSurrogateBackslash (accB <> char7 '\\') (accS + 1) rest high
      Just ('u', rest'') ->
        collectLowDigits (accB <> char7 '\\' <> charUtf8 'u') (accS + 2) rest'' rest high 0 0
      _ -> throwError (HQDecodeError InvalidSurrogatePair)
    _ -> throwError (HQDecodeError InvalidSurrogatePair)

-- | The low surrogate's backslash arrived at a chunk end. Mirrors
-- 'skipLowSurrogateBackslash'.
collectLowSurrogateBackslash ::
  Builder ->
  Int ->
  StreamIO Text () ->
  Int ->
  ExceptT HQError IO (Text, Builder, Int, StreamIO Text ())
collectLowSurrogateBackslash !accB !accS !text !high = do
  (chunk, rest) <- pullSkipText mempty text
  case T.uncons chunk of
    Nothing -> collectLowSurrogateBackslash accB accS rest high
    Just ('u', rest') -> collectLowDigits (accB <> charUtf8 'u') (accS + 1) rest' rest high 0 0
    _ -> throwError (HQDecodeError InvalidSurrogatePair)

-- | Capture the low surrogate's four hex digits. Mirrors
-- 'skipLowDigits'.
collectLowDigits ::
  Builder ->
  Int ->
  Text ->
  StreamIO Text () ->
  Int ->
  Int ->
  Int ->
  ExceptT HQError IO (Text, Builder, Int, StreamIO Text ())
collectLowDigits !accB !accS !input !text !high !value !digits
  | digits == 4 =
      if isLowSurrogate value
        then collectStringGo accB accS input text
        else throwError $ HQDecodeError InvalidSurrogatePair
  | otherwise = do
      (chunk, rest) <- pullSkipText input text
      case T.uncons chunk of
        Nothing -> collectLowDigits accB accS mempty rest high value digits
        Just _ -> do
          let remaining = 4 - digits
              limited = T.take remaining chunk
              (hexDigits, _) = T.span isHexDigit limited
              consumed = T.length hexDigits
              value' = T.foldl' (\ac c -> ac * 16 + digitToInt c) value hexDigits
              digits' = digits + consumed
              rest' = T.drop consumed chunk
              accB' = accB <> encodeUtf8Builder hexDigits
              accS' = accS + consumed
          if digits' == 4
            then
              if isLowSurrogate value'
                then collectStringGo accB' accS' rest' rest
                else throwError $ HQDecodeError InvalidSurrogatePair
            else
              if T.null rest'
                then collectLowDigits accB' accS' mempty rest high value' digits'
                else throwError $ HQDecodeError InvalidUnicodeEscape
