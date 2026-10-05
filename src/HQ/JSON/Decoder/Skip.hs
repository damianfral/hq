{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Text-level value skipping: validate like the decoder but
-- materialize nothing (no events, no 'Text's, no 'Scientific').
-- Discard-only: unlike the removed @collect*@ raw-capture path there
-- is no verbatim transcription here since 'takeValueChunks' is
-- canonical event-based output. Used by 'HQ.Runner.Cursor' for
-- container bodies ('skipContainerText') and member values
-- ('skipMemberValueText') when no replayed events are buffered;
-- anything else drains event by event.
module HQ.JSON.Decoder.Skip
  ( skipContainerText,
    skipMemberValueText,
    skipStringCollect,
    skipNumberCollect,
  )
where

import Control.Monad.Error.Class (MonadError (throwError))
import Data.ByteString.Builder (Builder, char7, charUtf8)
import Data.Char (digitToInt, isDigit, isHexDigit)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8Builder)
import HQ.Error (HQError (..))
import HQ.JSON.Decoder.Core
import HQ.JSON.Decoder.Error (DecodeError (..))
import HQ.JSON.Decoder.Number (advanceNumber, isValidNumberFinal, numberPhaseFromFirstChar)
import HQ.JSON.Event (JSONEvent (..))
import Relude hiding (Compose, id, many, some, state)
import qualified Streaming.Prelude as S

-- | Structural positions while skipping.
data SkipExpect
  = ExpectValue
  | ExpectKey
  | ExpectColon
  | ExpectObjComma
  | ExpectArrComma
  deriving (Eq, Show)

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
skipContainerText open decoder text = case open of
  JSONBeginArray -> skipRest ExpectValue
  JSONBeginObject -> skipRest ExpectKey
  _ -> pure (decoder, text)
  where
    -- The opener is already consumed, so the skip ends when the depth
    -- pops back below its entry level.
    skipRest expect = do
      let depth = decoderNestDepth decoder
          base = shallower depth
      (remainder, rest) <-
        skipExpect base depth (decoderStack decoder) expect (decoderInput decoder) text
      case decoderStack decoder of
        _ : ctxs ->
          pure (finishValue decoder {decoderInput = remainder, decoderStack = ctxs}, rest)
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
      (rest', text'') <- skipStringBody rest text'
      afterValue base depth stack rest' text''
    't' -> do
      (rest', text'') <- skipKeywordBody "true" 1 rest text'
      afterValue base depth stack rest' text''
    'f' -> do
      (rest', text'') <- skipKeywordBody "false" 1 rest text'
      afterValue base depth stack rest' text''
    'n' -> do
      (rest', text'') <- skipKeywordBody "null" 1 rest text'
      afterValue base depth stack rest' text''
    _
      | c == '-' || isDigit c -> do
          (rest', text'') <- skipNumberBody c rest text'
          afterValue base depth stack rest' text''
      | c == ']' -> case stack of
          DecodeArray : ctxs -> afterValue base (shallower depth) ctxs rest text'
          _ -> throwError (HQDecodeError (UnexpectedChar c))
      | otherwise -> throwError (HQDecodeError (UnexpectedChar c))
skipExpect base depth stack ExpectKey input text = do
  (c, rest, text') <- nextSkipChar input text
  case c of
    '}' -> case stack of
      _ : ctxs -> afterValue base (shallower depth) ctxs rest text'
      [] -> throwError (HQDecodeError ExpectedObjectKey)
    '"' -> do
      (rest', text'') <- skipStringBody rest text'
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

--------------------------------------------------------------------------------
-- Discard-only scalar skipping (no materialization)
--------------------------------------------------------------------------------

-- | Skip a string starting after its opening quote. Validates escapes,
-- unicode and surrogate pairs exactly like the decoder; bytes are
-- discarded, never captured.
skipStringBody ::
  Text -> StreamIO Text () -> ExceptT HQError IO (Text, StreamIO Text ())
skipStringBody = go
  where
    go !inp !txt = do
      let (_, rest) = T.span isStringChar inp
      case T.uncons rest of
        Nothing -> do
          (chunk, rest') <- pullSkipText mempty txt
          go chunk rest'
        Just (c, rest')
          | c == '"' -> pure (rest', txt)
          | c == '\\' -> skipEscape rest' txt
          | otherwise -> throwError (HQDecodeError (UnexpectedChar c))

-- | Skip one escape sequence (input starts after the backslash).
skipEscape ::
  Text -> StreamIO Text () -> ExceptT HQError IO (Text, StreamIO Text ())
skipEscape !input !text = do
  (chunk, rest) <- pullSkipText input text
  case T.uncons chunk of
    Nothing -> skipEscape mempty rest
    Just (c, rest')
      | c == 'u' -> skipUnicode rest' rest 0 0
      | isSimpleEscape c -> skipStringBody rest' rest
      | otherwise -> throwError (HQDecodeError (InvalidEscape c))

-- | Skip a @\u@ escape's four hex digits.
skipUnicode ::
  Text ->
  StreamIO Text () ->
  Int ->
  Int ->
  ExceptT HQError IO (Text, StreamIO Text ())
skipUnicode !input !text !value !digits = do
  (chunk, rest0) <- pullSkipText input text
  let (value', digits', rest') = accumulateHex value digits chunk
  if digits' >= 4
    then finishUnicode rest' rest0 value'
    else
      if T.null rest'
        then skipUnicode mempty rest0 value' digits'
        else throwError (HQDecodeError InvalidUnicodeEscape)

-- | Validate a completed @\u@ escape.
finishUnicode ::
  Text ->
  StreamIO Text () ->
  Int ->
  ExceptT HQError IO (Text, StreamIO Text ())
finishUnicode !input !text !value
  | isHighSurrogate value = skipLowSurrogate input text
  | isLowSurrogate value = throwError (HQDecodeError InvalidSurrogatePair)
  | otherwise = skipStringBody input text

-- | Skip a low-surrogate escape after a high surrogate.
skipLowSurrogate ::
  Text -> StreamIO Text () -> ExceptT HQError IO (Text, StreamIO Text ())
skipLowSurrogate !input !text = do
  (chunk, rest) <- pullSkipText input text
  case T.uncons chunk of
    Nothing -> skipLowSurrogate mempty rest
    Just ('\\', rest') -> case T.uncons rest' of
      Nothing -> skipLowBackslash rest
      Just ('u', rest'') -> skipLowDigits rest'' rest 0 0
      _ -> throwError (HQDecodeError InvalidSurrogatePair)
    _ -> throwError (HQDecodeError InvalidSurrogatePair)

-- | The low surrogate's backslash arrived at a chunk end.
skipLowBackslash ::
  StreamIO Text () -> ExceptT HQError IO (Text, StreamIO Text ())
skipLowBackslash !text = do
  (chunk, rest) <- pullSkipText mempty text
  case T.uncons chunk of
    Nothing -> skipLowBackslash rest
    Just ('u', rest') -> skipLowDigits rest' rest 0 0
    _ -> throwError (HQDecodeError InvalidSurrogatePair)

-- | Skip the low surrogate's four hex digits.
skipLowDigits ::
  Text ->
  StreamIO Text () ->
  Int ->
  Int ->
  ExceptT HQError IO (Text, StreamIO Text ())
skipLowDigits !input !text !value !digits = do
  (chunk, rest0) <- pullSkipText input text
  let (value', digits', rest') = accumulateHex value digits chunk
  if digits' >= 4
    then
      if isLowSurrogate value'
        then skipStringBody rest' rest0
        else throwError $ HQDecodeError InvalidSurrogatePair
    else
      if T.null rest'
        then skipLowDigits mempty rest0 value' digits'
        else throwError (HQDecodeError InvalidUnicodeEscape)

-- | Skip a number starting at its first character. The consumed text is
-- threaded through only for 'InvalidNumber' payloads (matching the
-- decoder); on success nothing is retained and no 'Scientific' is
-- built.
skipNumberBody ::
  Char ->
  Text ->
  StreamIO Text () ->
  ExceptT HQError IO (Text, StreamIO Text ())
skipNumberBody c inp txt =
  let phase = fromMaybe NumberSign (numberPhaseFromFirstChar c)
   in go phase (T.singleton c) inp txt
  where
    go !phase !buf !cur !stream
      | T.null cur = do
          result <- S.next stream
          case result of
            Left ()
              | isValidNumberFinal phase -> pure (mempty, stream)
              | otherwise -> throwError $ HQDecodeError $ InvalidNumber buf
            Right (chunk, rest) -> go phase buf chunk rest
      | otherwise = scan phase buf 0
      where
        len = T.length cur
        scan !ph !b !pos
          | pos >= len = go ph (b <> cur) mempty stream
          | otherwise =
              let ch = T.index cur pos
               in case advanceNumber ph ch of
                    NumberEnd
                      | isValidNumberFinal ph -> pure (T.drop pos cur, stream)
                      | otherwise ->
                          throwError $ HQDecodeError $ InvalidNumber (b <> T.take pos cur)
                    NumberError ->
                      throwError $ HQDecodeError $ InvalidNumber (b <> T.take pos cur <> T.singleton ch)
                    NumberStep ph' -> scan ph' b (pos + 1)

-- | Skip a keyword whose first character was consumed. A complete
-- keyword at exhaustion succeeds, mirroring the decoder.
skipKeywordBody ::
  Text ->
  Int ->
  Text ->
  StreamIO Text () ->
  ExceptT HQError IO (Text, StreamIO Text ())
skipKeywordBody keyword = go
  where
    go !index !inp !txt
      | index == T.length keyword = checkDelim inp txt
      | T.null inp = do
          result <- S.next txt
          case result of
            Left () -> throwError $ HQDecodeError $ InvalidKeyword keyword
            Right (chunk, rest) -> go index chunk rest
      | otherwise = case T.uncons inp of
          Nothing -> go index mempty txt
          Just (c, rest)
            | c == T.index keyword index -> go (index + 1) rest txt
            | otherwise -> throwError $ HQDecodeError $ InvalidKeyword keyword
    checkDelim !inp !txt
      | T.null inp = do
          result <- S.next txt
          case result of
            Left () -> pure (mempty, txt)
            Right (chunk, rest) -> checkDelim chunk rest
      | otherwise = case T.uncons inp of
          Nothing -> checkDelim mempty txt
          Just (c, _)
            | isJsonDelimiter c -> pure (inp, txt)
            | otherwise -> throwError (HQDecodeError (InvalidKeyword keyword))

--------------------------------------------------------------------------------
-- Raw capture (verbatim bytes for rewrite passthrough)
--------------------------------------------------------------------------------
--
-- Like skipping, but accumulating the consumed bytes so rewrite
-- passthrough can re-emit strings/numbers without decoding them.
-- Escapes stay verbatim, so bytes may differ from canonical re-encoding
-- while decoding to identical events.

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
     in collectNumber [T.singleton c] 1 phase rest text

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
                    $ InvalidNumber (T.concat (f <> [T.take pos inp]) <> T.singleton c)
                NumberStep p' -> loop f s p' (pos + 1)
    build = foldMap encodeUtf8Builder

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
      | c == 'u' -> collectUnicode (esc 'u') rest' rest 0 0
      | isSimpleEscape c -> collectString (esc c) rest' rest
      | otherwise -> throwError (HQDecodeError (InvalidEscape c))
  where
    esc c = (accB <> char7 '\\' <> charUtf8 c, accS + 2)
    collectString (b, s) = collectStringGo b s

-- | Capture a @\u@ escape's four hex digits. Single hex path via 'accumulateHex'.
collectUnicode ::
  (Builder, Int) ->
  Text ->
  StreamIO Text () ->
  Int ->
  Int ->
  ExceptT HQError IO (Text, Builder, Int, StreamIO Text ())
collectUnicode (!accB, !accS) !input !text !value !digits = do
  (chunk, rest0) <- pullSkipText input text
  let (value', digits', rest') = accumulateHex value digits chunk
      accB' = accB <> encodeUtf8Builder (T.take (T.length chunk - T.length rest') chunk)
      accS' = accS + (T.length chunk - T.length rest')
  if digits' >= 4
    then finishUnicodeCollect accB' accS' rest' rest0 value'
    else
      if T.null rest'
        then collectUnicode (accB', accS') mempty rest0 value' digits'
        else throwError (HQDecodeError InvalidUnicodeEscape)

-- | Validate a completed @\u@ escape.
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

-- | Capture a low-surrogate escape after a high surrogate.
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

-- | The low surrogate's backslash arrived at a chunk end.
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

-- | Capture the low surrogate's four hex digits.
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
