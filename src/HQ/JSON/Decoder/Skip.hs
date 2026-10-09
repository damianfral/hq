{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Text-level value skipping: validate like the decoder but
-- materialize nothing (no events, no 'Text's, no 'Scientific').
-- Two modes share the scan: discard-only ('skipContainerText',
-- 'skipMemberValueText') for bodies and member values with no
-- buffered replay, and verbatim raw-capture ('skipStringCollect',
-- 'skipNumberCollect') feeding the chunk fast path in Take.
-- Used by 'HQ.Runner.Cursor' for container bodies
-- ('skipContainerText') and member values ('skipMemberValueText')
-- when no replayed events are buffered; anything else drains event
-- by event.
module HQ.JSON.Decoder.Skip
  ( skipContainerText,
    skipMemberValueText,
    skipStringCollect,
    skipNumberCollect,
    collectExpect,
    finishContainer,
    SkipExpect (..),
    CollectPaused (..),
    collectBudget,
  )
where

import Control.Exception (throwIO)
import Data.Char (isDigit)
import Data.Text qualified as T
import GHC.Show (Show (showsPrec), showString)
import HQ.Early (Early, leave)
import HQ.Error (HQError (..))
import HQ.JSON.Decoder.Core
import HQ.JSON.Decoder.Error (DecodeError (..))
import HQ.JSON.Decoder.Number (acceptsDigitRun, advanceNumber, isValidNumberFinal, numberPhaseFromFirstChar)
import HQ.JSON.Event (JSONEvent (..))
import Relude hiding (Compose, id, many, some, state)
import Streaming.Prelude qualified as S

-- | Structural positions while skipping.
data SkipExpect
  = ExpectValue
  | ExpectKey
  | ExpectColon
  | ExpectObjComma
  | ExpectArrComma
  deriving (Eq, Show)

-- | Bodies collected past this size pause for emission: fragment
-- transients scale ~150x the body (measured), so collection proceeds
-- in bounded segments that emit verbatim and resume. Small values
-- (the common case) complete in one segment.
collectBudget :: Int
collectBudget = 16384

-- | A raw container collect paused for exceeding 'collectBudget': the
-- fragments so far (newest first) and the exact state to resume from
-- (position, depth context and live input). The caller emits the
-- fragments verbatim, then resumes; the concatenation is byte-identical
-- to one uninterrupted collect.
data CollectPaused
  = CollectPaused
      [Text]
      Int
      SkipExpect
      NestDepth
      NestDepth
      [DecodeContext]
      Text
      (StreamIO Text ())

instance Show CollectPaused where
  showsPrec _ _ = showString "CollectPaused: raw container segment ready, resuming collect"

instance Exception CollectPaused

-- | Pull the next chunk, skipping empties. Exhaustion here is
-- 'UnexpectedEnd'.
pullSkipText ::
  Early HQError ->
  Text ->
  StreamIO Text () ->
  IO (Text, StreamIO Text ())
pullSkipText early !input !text
  | T.null input = do
      result <- S.next text
      case result of
        Left () -> leave early (HQDecodeError UnexpectedEnd)
        Right (chunk, rest) -> pullSkipText early chunk rest
  | otherwise = pure (input, text)

-- | The next non-whitespace character, pulling more chunks as needed.
nextSkipChar ::
  Early HQError ->
  Text ->
  StreamIO Text () ->
  IO (Char, Text, StreamIO Text ())
nextSkipChar early input text = case T.uncons (T.dropWhile isWhitespace input) of
  Just (c, rest) -> pure (c, rest, text)
  Nothing -> do
    result <- S.next text
    case result of
      Left () -> leave early (HQDecodeError UnexpectedEnd)
      Right (chunk, rest) -> nextSkipChar early chunk rest

-- | Skip an object member value; a colon is required first.
skipMemberValueText ::
  Early HQError ->
  NestDepth ->
  [DecodeContext] ->
  Text ->
  StreamIO Text () ->
  IO (Text, StreamIO Text ())
skipMemberValueText early !depth !stack !input !text = do
  (c, rest, text') <- nextSkipChar early input text
  case c of
    ':' -> skipExpect early depth depth stack ExpectValue rest text'
    _ -> leave early (HQDecodeError ExpectedColon)

-- | Skip the rest of the container whose opening event was just emitted.
skipContainerText ::
  Early HQError ->
  JSONEvent ->
  DecoderState ->
  StreamIO Text () ->
  IO (DecoderState, StreamIO Text ())
skipContainerText early open decoder text = case open of
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
        skipExpect early base depth (decoderStack decoder) expect (decoderInput decoder) text
      case decoderStack decoder of
        _ : ctxs ->
          pure (finishValue decoder {decoderInput = remainder, decoderStack = ctxs}, rest)
        [] -> pure (finishValue decoder {decoderInput = remainder}, rest)

-- | Pop one container context and mark the parent, after the
-- container's closing bracket was consumed. Shared by every raw
-- container collect, however many segments it took.
finishContainer :: DecoderState -> Text -> DecoderState
finishContainer decoder remainder = case decoderStack decoder of
  _ : ctxs -> finishValue decoder {decoderInput = remainder, decoderStack = ctxs}
  [] -> finishValue decoder {decoderInput = remainder}

-- | Structural collect loop; ends when the depth pops back to its
-- entry level. Mirror of 'skipExpect' accumulating raw bytes instead
-- of discarding: same states, same errors, same termination.
collectExpect ::
  Early HQError ->
  [Text] ->
  Int ->
  NestDepth ->
  NestDepth ->
  [DecodeContext] ->
  SkipExpect ->
  Text ->
  StreamIO Text () ->
  IO (Text, [Text], Int, StreamIO Text ())
collectExpect early acc accSize base depth stack ExpectValue input text
  | accSize > collectBudget = throwIO (CollectPaused acc accSize ExpectValue base depth stack input text)
  | otherwise = do
      (c, rest, text') <- nextSkipChar early input text
      case c of
        '{' -> collectExpect early (T.singleton '{' : acc) (accSize + 1) base (deeper depth) (DecodeObject : stack) ExpectKey rest text'
        '[' -> collectExpect early (T.singleton '[' : acc) (accSize + 1) base (deeper depth) (DecodeArray : stack) ExpectValue rest text'
        '"' -> do
          (rest', frags, size, text'') <- skipStringCollect early rest text'
          collectAfterValue early (frags <> acc) (size + accSize) base depth stack rest' text''
        't' -> do
          (rest', frags, size, text'') <- skipKeywordCollect early rest text' "true"
          collectAfterValue early (frags <> acc) (size + accSize) base depth stack rest' text''
        'f' -> do
          (rest', frags, size, text'') <- skipKeywordCollect early rest text' "false"
          collectAfterValue early (frags <> acc) (size + accSize) base depth stack rest' text''
        'n' -> do
          (rest', frags, size, text'') <- skipKeywordCollect early rest text' "null"
          collectAfterValue early (frags <> acc) (size + accSize) base depth stack rest' text''
        _
          | c == '-' || isDigit c -> do
              let phase = fromMaybe NumberSign (numberPhaseFromFirstChar c)
              (rest', frags, size, text'') <- collectNumber early [T.singleton c] 1 phase rest text'
              collectAfterValue early (frags <> acc) (size + accSize) base depth stack rest' text''
          | c == ']' -> case stack of
              DecodeArray : ctxs -> collectAfterValue early (T.singleton ']' : acc) (accSize + 1) base (shallower depth) ctxs rest text'
              _ -> leave early (HQDecodeError (UnexpectedChar c))
          | otherwise -> leave early (HQDecodeError (UnexpectedChar c))
collectExpect early acc accSize base depth stack ExpectKey input text
  | accSize > collectBudget = throwIO (CollectPaused acc accSize ExpectKey base depth stack input text)
  | otherwise = do
      (c, rest, text') <- nextSkipChar early input text
      case c of
        '}' -> case stack of
          _ : ctxs -> collectAfterValue early (T.singleton '}' : acc) (accSize + 1) base (shallower depth) ctxs rest text'
          [] -> leave early (HQDecodeError ExpectedObjectKey)
        '"' -> do
          (rest', frags, size, text'') <- skipStringCollect early rest text'
          collectExpect early (frags <> acc) (size + accSize) base depth stack ExpectColon rest' text''
        _ -> leave early (HQDecodeError ExpectedObjectKey)
collectExpect early acc accSize base depth stack ExpectColon input text
  | accSize > collectBudget = throwIO (CollectPaused acc accSize ExpectColon base depth stack input text)
  | otherwise = do
      (c, rest, text') <- nextSkipChar early input text
      case c of
        ':' -> collectExpect early (T.singleton ':' : acc) (accSize + 1) base depth stack ExpectValue rest text'
        _ -> leave early (HQDecodeError ExpectedColon)
collectExpect early acc accSize base depth stack ExpectObjComma input text
  | accSize > collectBudget = throwIO (CollectPaused acc accSize ExpectObjComma base depth stack input text)
  | otherwise = do
      (c, rest, text') <- nextSkipChar early input text
      case c of
        ',' -> collectExpect early (T.singleton ',' : acc) (accSize + 1) base depth stack ExpectKey rest text'
        '}' -> case stack of
          _ : ctxs -> collectAfterValue early (T.singleton '}' : acc) (accSize + 1) base (shallower depth) ctxs rest text'
          [] -> leave early (HQDecodeError ExpectedCommaOrEnd)
        _ -> leave early (HQDecodeError ExpectedCommaOrEnd)
collectExpect early acc accSize base depth stack ExpectArrComma input text
  | accSize > collectBudget = throwIO (CollectPaused acc accSize ExpectArrComma base depth stack input text)
  | otherwise = do
      (c, rest, text') <- nextSkipChar early input text
      case c of
        ',' -> collectExpect early (T.singleton ',' : acc) (accSize + 1) base depth stack ExpectValue rest text'
        ']' -> case stack of
          _ : ctxs -> collectAfterValue early (T.singleton ']' : acc) (accSize + 1) base (shallower depth) ctxs rest text'
          [] -> leave early (HQDecodeError ExpectedCommaOrEnd)
        _ -> leave early (HQDecodeError ExpectedCommaOrEnd)

-- | A value just completed: return at entry depth, else expect the
-- enclosing separator. Mirror of 'afterValue'.
collectAfterValue ::
  Early HQError ->
  [Text] ->
  Int ->
  NestDepth ->
  NestDepth ->
  [DecodeContext] ->
  Text ->
  StreamIO Text () ->
  IO (Text, [Text], Int, StreamIO Text ())
collectAfterValue early acc accSize base depth stack rest text'
  | depth <= base = pure (rest, acc, accSize, text')
  | otherwise = case stack of
      DecodeArray : _ -> collectExpect early acc accSize base depth stack ExpectArrComma rest text'
      DecodeObject : _ -> collectExpect early acc accSize base depth stack ExpectObjComma rest text'
      [] -> pure (rest, acc, accSize, text')

-- | Structural skip loop; ends when the depth pops back to its entry level.
skipExpect ::
  Early HQError ->
  NestDepth ->
  NestDepth ->
  [DecodeContext] ->
  SkipExpect ->
  Text ->
  StreamIO Text () ->
  IO (Text, StreamIO Text ())
skipExpect early base depth stack ExpectValue input text = do
  (c, rest, text') <- nextSkipChar early input text
  case c of
    '{' -> skipExpect early base (deeper depth) (DecodeObject : stack) ExpectKey rest text'
    '[' -> skipExpect early base (deeper depth) (DecodeArray : stack) ExpectValue rest text'
    '"' -> do
      (rest', text'') <- skipStringBody early rest text'
      afterValue early base depth stack rest' text''
    't' -> do
      (rest', text'') <- skipKeywordBody early "true" 1 rest text'
      afterValue early base depth stack rest' text''
    'f' -> do
      (rest', text'') <- skipKeywordBody early "false" 1 rest text'
      afterValue early base depth stack rest' text''
    'n' -> do
      (rest', text'') <- skipKeywordBody early "null" 1 rest text'
      afterValue early base depth stack rest' text''
    _
      | c == '-' || isDigit c -> do
          (rest', text'') <- skipNumberBody early c rest text'
          afterValue early base depth stack rest' text''
      | c == ']' -> case stack of
          DecodeArray : ctxs -> afterValue early base (shallower depth) ctxs rest text'
          _ -> leave early (HQDecodeError (UnexpectedChar c))
      | otherwise -> leave early (HQDecodeError (UnexpectedChar c))
skipExpect early base depth stack ExpectKey input text = do
  (c, rest, text') <- nextSkipChar early input text
  case c of
    '}' -> case stack of
      _ : ctxs -> afterValue early base (shallower depth) ctxs rest text'
      [] -> leave early (HQDecodeError ExpectedObjectKey)
    '"' -> do
      (rest', text'') <- skipStringBody early rest text'
      skipExpect early base depth stack ExpectColon rest' text''
    _ -> leave early (HQDecodeError ExpectedObjectKey)
skipExpect early base depth stack ExpectColon input text = do
  (c, rest, text') <- nextSkipChar early input text
  case c of
    ':' -> skipExpect early base depth stack ExpectValue rest text'
    _ -> leave early (HQDecodeError ExpectedColon)
skipExpect early base depth stack ExpectObjComma input text = do
  (c, rest, text') <- nextSkipChar early input text
  case c of
    ',' -> skipExpect early base depth stack ExpectKey rest text'
    '}' -> case stack of
      _ : ctxs -> afterValue early base (shallower depth) ctxs rest text'
      [] -> leave early (HQDecodeError ExpectedCommaOrEnd)
    _ -> leave early (HQDecodeError ExpectedCommaOrEnd)
skipExpect early base depth stack ExpectArrComma input text = do
  (c, rest, text') <- nextSkipChar early input text
  case c of
    ',' -> skipExpect early base depth stack ExpectValue rest text'
    ']' -> case stack of
      _ : ctxs -> afterValue early base (shallower depth) ctxs rest text'
      [] -> leave early (HQDecodeError ExpectedCommaOrEnd)
    _ -> leave early (HQDecodeError ExpectedCommaOrEnd)

-- | A value just completed: return at entry depth, else expect the
-- enclosing separator.
afterValue ::
  Early HQError ->
  NestDepth ->
  NestDepth ->
  [DecodeContext] ->
  Text ->
  StreamIO Text () ->
  IO (Text, StreamIO Text ())
afterValue early base depth stack rest text'
  | depth <= base = pure (rest, text')
  | otherwise = case stack of
      DecodeArray : _ -> skipExpect early base depth stack ExpectArrComma rest text'
      DecodeObject : _ -> skipExpect early base depth stack ExpectObjComma rest text'
      [] -> pure (rest, text')

--------------------------------------------------------------------------------
-- Discard-only scalar skipping (no materialization)
--------------------------------------------------------------------------------

-- | Skip a string starting after its opening quote. Validates escapes,
-- unicode and surrogate pairs exactly like the decoder; bytes are
-- discarded, never captured.
skipStringBody ::
  Early HQError ->
  Text ->
  StreamIO Text () ->
  IO (Text, StreamIO Text ())
skipStringBody early = go
  where
    go !inp !txt = do
      let (_, rest) = T.span isStringChar inp
      case T.uncons rest of
        Nothing -> do
          (chunk, rest') <- pullSkipText early mempty txt
          go chunk rest'
        Just (c, rest')
          | c == '"' -> pure (rest', txt)
          | c == '\\' -> skipEscape early rest' txt
          | otherwise -> leave early (HQDecodeError (UnexpectedChar c))

-- | Skip one escape sequence (input starts after the backslash).
skipEscape ::
  Early HQError ->
  Text ->
  StreamIO Text () ->
  IO (Text, StreamIO Text ())
skipEscape early !input !text = do
  (chunk, rest) <- pullSkipText early input text
  case T.uncons chunk of
    Nothing -> skipEscape early mempty rest
    Just (c, rest')
      | c == 'u' -> skipUnicode early rest' rest 0 0
      | isSimpleEscape c -> skipStringBody early rest' rest
      | otherwise -> leave early (HQDecodeError (InvalidEscape c))

-- | Skip a @\u@ escape's four hex digits.
skipUnicode ::
  Early HQError ->
  Text ->
  StreamIO Text () ->
  Int ->
  Int ->
  IO (Text, StreamIO Text ())
skipUnicode early !input !text !value !digits = do
  (chunk, rest0) <- pullSkipText early input text
  let (value', digits', rest') = accumulateHex value digits chunk
  if digits' >= 4
    then finishUnicode early rest' rest0 value'
    else
      if T.null rest'
        then skipUnicode early mempty rest0 value' digits'
        else leave early (HQDecodeError InvalidUnicodeEscape)

-- | Validate a completed @\u@ escape.
finishUnicode ::
  Early HQError ->
  Text ->
  StreamIO Text () ->
  Int ->
  IO (Text, StreamIO Text ())
finishUnicode early !input !text !value
  | isHighSurrogate value = skipLowSurrogate early input text
  | isLowSurrogate value = leave early (HQDecodeError InvalidSurrogatePair)
  | otherwise = skipStringBody early input text

-- | Skip a low-surrogate escape after a high surrogate.
skipLowSurrogate ::
  Early HQError ->
  Text ->
  StreamIO Text () ->
  IO (Text, StreamIO Text ())
skipLowSurrogate early !input !text = do
  (chunk, rest) <- pullSkipText early input text
  case T.uncons chunk of
    Nothing -> skipLowSurrogate early mempty rest
    Just ('\\', rest') -> case T.uncons rest' of
      Nothing -> skipLowBackslash early rest
      Just ('u', rest'') -> skipLowDigits early rest'' rest 0 0
      _ -> leave early (HQDecodeError InvalidSurrogatePair)
    _ -> leave early (HQDecodeError InvalidSurrogatePair)

-- | The low surrogate's backslash arrived at a chunk end.
skipLowBackslash ::
  Early HQError ->
  StreamIO Text () ->
  IO (Text, StreamIO Text ())
skipLowBackslash early !text = do
  (chunk, rest) <- pullSkipText early mempty text
  case T.uncons chunk of
    Nothing -> skipLowBackslash early rest
    Just ('u', rest') -> skipLowDigits early rest' rest 0 0
    _ -> leave early (HQDecodeError InvalidSurrogatePair)

-- | Skip the low surrogate's four hex digits.
skipLowDigits ::
  Early HQError ->
  Text ->
  StreamIO Text () ->
  Int ->
  Int ->
  IO (Text, StreamIO Text ())
skipLowDigits early !input !text !value !digits = do
  (chunk, rest0) <- pullSkipText early input text
  let (value', digits', rest') = accumulateHex value digits chunk
  if digits' >= 4
    then
      if isLowSurrogate value'
        then skipStringBody early rest' rest0
        else leave early $ HQDecodeError InvalidSurrogatePair
    else
      if T.null rest'
        then skipLowDigits early mempty rest0 value' digits'
        else leave early (HQDecodeError InvalidUnicodeEscape)

-- | Skip a number starting at its first character. Consumed fragments
-- are threaded through (newest first) only for 'InvalidNumber'
-- payloads, concatenated on error; on success nothing is retained and
-- no 'Scientific' is built. Digit runs in self-looping phases are
-- consumed with a single span instead of per-character dispatch.
skipNumberBody ::
  Early HQError ->
  Char ->
  Text ->
  StreamIO Text () ->
  IO (Text, StreamIO Text ())
skipNumberBody early c inp txt =
  let phase = fromMaybe NumberSign (numberPhaseFromFirstChar c)
   in go phase [T.singleton c] inp txt
  where
    go !phase !frags !cur !stream
      | T.null cur = do
          result <- S.next stream
          case result of
            Left ()
              | isValidNumberFinal phase -> pure (mempty, stream)
              | otherwise -> leave early $ HQDecodeError $ InvalidNumber (T.concat (reverse frags))
            Right (chunk, rest) -> go phase frags chunk rest
      | otherwise = scan phase frags 0
      where
        len = T.length cur
        scan !ph !fs !pos
          | pos >= len = go ph (cur : fs) mempty stream
          | acceptsDigitRun ph = runDigits ph fs pos
          | otherwise = stepChar ph fs pos
        -- Digits self-loop in these phases, so the phase is unchanged
        -- after a maximal run; only the terminating character needs
        -- the full dispatch.
        runDigits !ph' !fs' !pos' =
          let pos'' = pos' + T.length (T.takeWhile isDigit (T.drop pos' cur))
           in if pos'' >= len
                then go ph' (cur : fs') mempty stream
                else stepChar ph' fs' pos''
        stepChar !ph' !fs' !pos' =
          let ch = T.index cur pos'
           in case advanceNumber ph' ch of
                NumberEnd
                  | isValidNumberFinal ph' -> pure (T.drop pos' cur, stream)
                  | otherwise ->
                      leave early $ HQDecodeError $ InvalidNumber (T.concat (reverse fs') <> T.take pos' cur)
                NumberError ->
                  leave early $ HQDecodeError $ InvalidNumber (T.concat (reverse fs') <> T.take pos' cur <> T.singleton ch)
                NumberStep ph'' -> scan ph'' fs' (pos' + 1)

-- | Skip a keyword whose first character was consumed. A complete
-- keyword at exhaustion succeeds, mirroring the decoder.
skipKeywordBody ::
  Early HQError ->
  Text ->
  Int ->
  Text ->
  StreamIO Text () ->
  IO (Text, StreamIO Text ())
skipKeywordBody early keyword = go
  where
    go !index !inp !txt
      | index == T.length keyword = checkDelim inp txt
      | T.null inp = do
          result <- S.next txt
          case result of
            Left () -> leave early $ HQDecodeError $ InvalidKeyword keyword
            Right (chunk, rest) -> go index chunk rest
      | otherwise = case T.uncons inp of
          Nothing -> go index mempty txt
          Just (c, rest)
            | c == T.index keyword index -> go (index + 1) rest txt
            | otherwise -> leave early $ HQDecodeError $ InvalidKeyword keyword
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
            | otherwise -> leave early (HQDecodeError (InvalidKeyword keyword))

-- | Skip a keyword, capturing its raw bytes. Mirrors 'skipKeywordBody';
-- the literal is fixed, so it is returned whole on success.
skipKeywordCollect ::
  Early HQError ->
  Text ->
  StreamIO Text () ->
  Text ->
  IO (Text, [Text], Int, StreamIO Text ())
skipKeywordCollect early input text keyword = go 1 input text
  where
    go !ix !inp !txt
      | ix == T.length keyword = checkDelim inp txt
      | T.null inp = do
          result <- S.next txt
          case result of
            Left () -> leave early $ HQDecodeError $ InvalidKeyword keyword
            Right (chunk, rest) -> go ix chunk rest
      | otherwise = case T.uncons inp of
          Nothing -> go ix mempty txt
          Just (c, rest)
            | c == T.index keyword ix -> go (ix + 1) rest txt
            | otherwise -> leave early $ HQDecodeError $ InvalidKeyword keyword
    checkDelim !inp !txt
      | T.null inp = do
          result <- S.next txt
          case result of
            Left () -> pure (mempty, [keyword], T.length keyword, txt)
            Right (chunk, rest) -> checkDelim chunk rest
      | otherwise = case T.uncons inp of
          Nothing -> checkDelim mempty txt
          Just (c, _)
            | isJsonDelimiter c ->
                pure (inp, [keyword], T.length keyword, txt)
            | otherwise -> leave early (HQDecodeError (InvalidKeyword keyword))

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
  Early HQError ->
  Text ->
  StreamIO Text () ->
  IO (Text, [Text], Int, StreamIO Text ())
skipNumberCollect early input text = case T.uncons input of
  Nothing -> do
    (chunk, rest) <- pullSkipText early mempty text
    skipNumberCollect early chunk rest
  Just (c, rest) ->
    let phase = fromMaybe NumberSign (numberPhaseFromFirstChar c)
     in collectNumber early [T.singleton c] 1 phase rest text

collectNumber ::
  Early HQError ->
  [Text] ->
  Int ->
  NumberPhase ->
  Text ->
  StreamIO Text () ->
  IO (Text, [Text], Int, StreamIO Text ())
collectNumber early frags size phase inp txt
  | T.null inp = do
      result <- S.next txt
      case result of
        Left ()
          | isValidNumberFinal phase -> pure (mempty, frags, size, txt)
          | otherwise ->
              leave early $ HQDecodeError $ InvalidNumber (T.concat (reverse frags))
        Right (chunk, rest) -> collectNumber early frags size phase chunk rest
  | otherwise = loop frags size phase 0
  where
    len = T.length inp
    loop f s p pos
      | pos >= len = collectNumber early (inp : f) (s + len) p mempty txt
      | acceptsDigitRun p = runDigits f s p pos
      | otherwise = stepChar f s p pos
    -- Digits self-loop in these phases: consume the maximal run with
    -- one span, then dispatch once on the terminating character.
    runDigits f' s' p' pos' =
      let pos'' = pos' + T.length (T.takeWhile isDigit (T.drop pos' inp))
       in if pos'' >= len
            then collectNumber early (inp : f') (s' + len) p' mempty txt
            else stepChar f' s' p' pos''
    stepChar f' s' p' pos' =
      let c = T.index inp pos'
       in case advanceNumber p' c of
            NumberEnd
              | isValidNumberFinal p' ->
                  let frags' = T.take pos' inp : f'
                   in pure (T.drop pos' inp, frags', s' + pos', txt)
              | otherwise ->
                  leave early
                    $ HQDecodeError
                    $ InvalidNumber (T.concat (reverse f' <> [T.take pos' inp]))
            NumberError ->
              leave early
                $ HQDecodeError
                $ InvalidNumber (T.concat (reverse f' <> [T.take pos' inp]) <> T.singleton c)
            NumberStep p'' -> loop f' s' p'' (pos' + 1)

-- | Skip a string, capturing its raw bytes, quotes included. The
-- opening quote seeds the fragments (oldest position); the closing
-- quote is consed at the end. Newest first, like the number frags.
skipStringCollect ::
  Early HQError ->
  Text ->
  StreamIO Text () ->
  IO (Text, [Text], Int, StreamIO Text ())
skipStringCollect early = collectStringGo early [T.singleton '"'] 1

collectStringGo ::
  Early HQError ->
  [Text] ->
  Int ->
  Text ->
  StreamIO Text () ->
  IO (Text, [Text], Int, StreamIO Text ())
collectStringGo early !acc !accSize !inp !txt = do
  let (safe, rest) = T.span isStringChar inp
      acc' = safe : acc
      accSize' = accSize + T.length safe
  case T.uncons rest of
    Nothing -> do
      (chunk, rest') <- pullSkipText early mempty txt
      collectStringGo early acc' accSize' chunk rest'
    Just (c, rest')
      | c == '"' -> pure (rest', T.singleton '"' : acc', accSize' + 1, txt)
      | c == '\\' -> collectEscape early acc' accSize' rest' txt
      | otherwise -> leave early (HQDecodeError (UnexpectedChar c))

-- | Capture one escape sequence, prepending its raw bytes.
collectEscape ::
  Early HQError ->
  [Text] ->
  Int ->
  Text ->
  StreamIO Text () ->
  IO (Text, [Text], Int, StreamIO Text ())
collectEscape early !acc !accSize !input !text = do
  (chunk, rest) <- pullSkipText early input text
  case T.uncons chunk of
    Nothing -> collectEscape early acc accSize mempty rest
    Just (c, rest')
      | c == 'u' -> collectUnicode early (esc 'u') rest' rest 0 0
      | isSimpleEscape c -> collectString (esc c) rest' rest
      | otherwise -> leave early (HQDecodeError (InvalidEscape c))
  where
    esc c = (T.pack ['\\', c] : acc, accSize + 2)
    collectString (b, s) = collectStringGo early b s

-- | Capture a @\u@ escape's four hex digits. Single hex path via 'accumulateHex'.
collectUnicode ::
  Early HQError ->
  ([Text], Int) ->
  Text ->
  StreamIO Text () ->
  Int ->
  Int ->
  IO (Text, [Text], Int, StreamIO Text ())
collectUnicode early (!acc, !accSize) !input !text !value !digits = do
  (chunk, rest0) <- pullSkipText early input text
  let (value', digits', rest') = accumulateHex value digits chunk
      acc' = T.take (T.length chunk - T.length rest') chunk : acc
      accSize' = accSize + (T.length chunk - T.length rest')
  if digits' >= 4
    then finishUnicodeCollect early acc' accSize' rest' rest0 value'
    else
      if T.null rest'
        then collectUnicode early (acc', accSize') mempty rest0 value' digits'
        else leave early (HQDecodeError InvalidUnicodeEscape)

-- | Validate a completed @\u@ escape.
finishUnicodeCollect ::
  Early HQError ->
  [Text] ->
  Int ->
  Text ->
  StreamIO Text () ->
  Int ->
  IO (Text, [Text], Int, StreamIO Text ())
finishUnicodeCollect early !acc !accSize !input !text !value
  | isHighSurrogate value = collectLowSurrogate early acc accSize input text value
  | isLowSurrogate value = leave early (HQDecodeError InvalidSurrogatePair)
  | otherwise = collectStringGo early acc accSize input text

-- | Capture a low-surrogate escape after a high surrogate.
collectLowSurrogate ::
  Early HQError ->
  [Text] ->
  Int ->
  Text ->
  StreamIO Text () ->
  Int ->
  IO (Text, [Text], Int, StreamIO Text ())
collectLowSurrogate early !acc !accSize !input !text !high = do
  (chunk, rest) <- pullSkipText early input text
  case T.uncons chunk of
    Nothing -> collectLowSurrogate early acc accSize mempty rest high
    Just ('\\', rest') -> case T.uncons rest' of
      Nothing -> collectLowSurrogateBackslash early (T.singleton '\\' : acc) (accSize + 1) rest high
      Just ('u', rest'') ->
        collectLowDigits early (T.pack ['\\', 'u'] : acc) (accSize + 2) rest'' rest high 0 0
      _ -> leave early (HQDecodeError InvalidSurrogatePair)
    _ -> leave early (HQDecodeError InvalidSurrogatePair)

-- | The low surrogate's backslash arrived at a chunk end.
collectLowSurrogateBackslash ::
  Early HQError ->
  [Text] ->
  Int ->
  StreamIO Text () ->
  Int ->
  IO (Text, [Text], Int, StreamIO Text ())
collectLowSurrogateBackslash early !acc !accSize !text !high = do
  (chunk, rest) <- pullSkipText early mempty text
  case T.uncons chunk of
    Nothing -> collectLowSurrogateBackslash early acc accSize rest high
    Just ('u', rest') -> collectLowDigits early (T.singleton 'u' : acc) (accSize + 1) rest' rest high 0 0
    _ -> leave early (HQDecodeError InvalidSurrogatePair)

-- | Capture the low surrogate's four hex digits.
collectLowDigits ::
  Early HQError ->
  [Text] ->
  Int ->
  Text ->
  StreamIO Text () ->
  Int ->
  Int ->
  Int ->
  IO (Text, [Text], Int, StreamIO Text ())
collectLowDigits early !acc !accSize !input !text !high !value !digits
  | digits == 4 =
      if isLowSurrogate value
        then collectStringGo early acc accSize input text
        else leave early $ HQDecodeError InvalidSurrogatePair
  | otherwise = do
      (chunk, rest) <- pullSkipText early input text
      case T.uncons chunk of
        Nothing -> collectLowDigits early acc accSize mempty rest high value digits
        Just _ -> do
          let (value', digits', rest') = accumulateHex value digits chunk
              consumed = digits' - digits
              acc' = T.take consumed chunk : acc
              accSize' = accSize + consumed
          if digits' == 4
            then
              if isLowSurrogate value'
                then collectStringGo early acc' accSize' rest' rest
                else leave early $ HQDecodeError InvalidSurrogatePair
            else
              if T.null rest'
                then collectLowDigits early acc' accSize' mempty rest high value' digits'
                else leave early $ HQDecodeError InvalidUnicodeEscape
