{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Text-level value skipping: validate like the decoder but materialize nothing.
module HQ.JSON.Decoder.Skip
  ( skipContainerText,
    skipMemberValueText,
    skipStringCollect,
    skipNumberCollect,
    collectExpect,
    finishContainer,
    SkipExpect (..),
    SkipRest (..),
    Collected (..),
    CollectPaused (..),
    RawBody (..),
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
import HQ.JSON.Decoder.Number
import HQ.JSON.Event (JSONEvent (..))
import Relude hiding (Compose, id, many, some, state)
import Streaming.Prelude qualified as S

-- | Raw collected bytes: fragments newest-first with their total character count.
data RawBody = RawBody {rawFrags :: [Text], rawSize :: Int}

-- | Where decoding stands: pending input text and the live text stream after it.
data SkipRest = SkipRest
  { skipRestInput :: Text,
    skipRestStream :: StreamIO Text ()
  }

-- | Result of collecting one value: where decoding stands and the collected body.
data Collected = Collected
  { collectedRest :: SkipRest,
    collectedBody :: RawBody
  }

instance Semigroup RawBody where
  RawBody f1 s1 <> RawBody f2 s2 = RawBody (f1 <> f2) (s1 + s2)

instance Monoid RawBody where mempty = RawBody [] 0

-- | One collected character.
rawChar :: Char -> RawBody
rawChar c = RawBody [T.singleton c] 1

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
-- body so far and the exact state to resume from (position, depth
-- context and live input). The caller emits the body verbatim, then
-- resumes; the concatenation is byte-identical to one uninterrupted
-- collect.
data CollectPaused = CollectPaused
  { pausedBody :: RawBody,
    pausedExpect :: SkipExpect,
    pausedBase :: NestDepth,
    pausedDepth :: NestDepth,
    pausedStack :: [DecodeContext],
    pausedInput :: Text,
    pausedStream :: StreamIO Text ()
  }

instance Show CollectPaused where
  showsPrec _ _ = showString "CollectPaused: raw container segment ready, resuming collect"

instance Exception CollectPaused

-- | Pull the next chunk, skipping empties. Exhaustion here is
-- 'UnexpectedEnd'.
pullSkipText :: Early HQError -> Text -> StreamIO Text () -> IO SkipRest
pullSkipText early !input !text
  | T.null input = do
      result <- S.next text
      case result of
        Left () -> leave early (HQDecodeError UnexpectedEnd)
        Right (chunk, rest) -> pullSkipText early chunk rest
  | otherwise = pure (SkipRest input text)

-- | The next non-whitespace character, pulling more chunks as needed.
nextSkipChar :: Early HQError -> Text -> StreamIO Text () -> IO (Char, SkipRest)
nextSkipChar early input text = case T.uncons (T.dropWhile isWhitespace input) of
  Just (c, rest) -> pure (c, SkipRest rest text)
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
  IO SkipRest
skipMemberValueText early !depth !stack !input !text = do
  (c, SkipRest rest text') <- nextSkipChar early input text
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
      (SkipRest remainder rest) <-
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
  RawBody ->
  NestDepth ->
  NestDepth ->
  [DecodeContext] ->
  SkipExpect ->
  Text ->
  StreamIO Text () ->
  IO Collected
collectExpect early body base depth stack ExpectValue input text
  | rawSize body > collectBudget =
      throwIO (CollectPaused body ExpectValue base depth stack input text)
  | otherwise = do
      (c, SkipRest rest text') <- nextSkipChar early input text
      case c of
        '{' ->
          let rawBody = (rawChar '{' <> body)
              ctxs = DecodeObject : stack
              d = deeper depth
           in collectExpect early rawBody base d ctxs ExpectKey rest text'
        '[' ->
          let rawBody = rawChar '[' <> body
              ctxs = DecodeArray : stack
              d = deeper depth
           in collectExpect early rawBody base d ctxs ExpectValue rest text'
        '"' -> do
          (Collected (SkipRest rest' text'') body') <-
            skipStringCollect early rest text'
          collectAfterValue early (body' <> body) base depth stack rest' text''
        't' -> do
          (Collected (SkipRest rest' text'') body') <-
            skipKeywordCollect early rest text' "true"
          collectAfterValue early (body' <> body) base depth stack rest' text''
        'f' -> do
          (Collected (SkipRest rest' text'') body') <-
            skipKeywordCollect early rest text' "false"
          collectAfterValue early (body' <> body) base depth stack rest' text''
        'n' -> do
          (Collected (SkipRest rest' text'') body') <-
            skipKeywordCollect early rest text' "null"
          collectAfterValue early (body' <> body) base depth stack rest' text''
        _
          | c == '-' || isDigit c -> do
              let phase = fromMaybe NumberSign (numberPhaseFromFirstChar c)
              (Collected (SkipRest rest' text'') body') <-
                collectNumber early (rawChar c) phase rest text'
              let resultBody = body' <> body
              collectAfterValue early resultBody base depth stack rest' text''
          | c == ']' -> case stack of
              DecodeArray : ctxs ->
                let rawBody = rawChar ']' <> body
                    d = shallower depth
                 in collectAfterValue early rawBody base d ctxs rest text'
              _ -> leave early (HQDecodeError (UnexpectedChar c))
          | otherwise -> leave early (HQDecodeError (UnexpectedChar c))
collectExpect early body base depth stack ExpectKey input text
  | rawSize body > collectBudget =
      throwIO (CollectPaused body ExpectKey base depth stack input text)
  | otherwise = do
      (c, SkipRest rest text') <- nextSkipChar early input text
      case c of
        '}' -> case stack of
          _ : ctxs ->
            let rawBody = rawChar '}' <> body
                d = shallower depth
             in collectAfterValue early rawBody base d ctxs rest text'
          [] -> leave early (HQDecodeError ExpectedObjectKey)
        '"' -> do
          (Collected (SkipRest rest' text'') body') <-
            skipStringCollect early rest text'
          let resultBody = body' <> body
          collectExpect early resultBody base depth stack ExpectColon rest' text''
        _ -> leave early (HQDecodeError ExpectedObjectKey)
collectExpect early body base depth stack ExpectColon input text
  | rawSize body > collectBudget =
      throwIO (CollectPaused body ExpectColon base depth stack input text)
  | otherwise = do
      (c, SkipRest rest text') <- nextSkipChar early input text
      case c of
        ':' ->
          let rawBody = rawChar ':' <> body
           in collectExpect early rawBody base depth stack ExpectValue rest text'
        _ -> leave early (HQDecodeError ExpectedColon)
collectExpect early body base depth stack ExpectObjComma input text
  | rawSize body > collectBudget =
      throwIO (CollectPaused body ExpectObjComma base depth stack input text)
  | otherwise = do
      (c, SkipRest rest text') <- nextSkipChar early input text
      case c of
        ',' ->
          let rawBody = rawChar ',' <> body
           in collectExpect early rawBody base depth stack ExpectKey rest text'
        '}' -> case stack of
          _ : ctxs ->
            let rawBody = rawChar '}' <> body
                d = shallower depth
             in collectAfterValue early rawBody base d ctxs rest text'
          [] -> leave early (HQDecodeError ExpectedCommaOrEnd)
        _ -> leave early (HQDecodeError ExpectedCommaOrEnd)
collectExpect early body base depth stack ExpectArrComma input text
  | rawSize body > collectBudget =
      throwIO (CollectPaused body ExpectArrComma base depth stack input text)
  | otherwise = do
      (c, SkipRest rest text') <- nextSkipChar early input text
      case c of
        ',' ->
          let rawBody = rawChar ',' <> body
           in collectExpect early rawBody base depth stack ExpectValue rest text'
        ']' -> case stack of
          _ : ctxs ->
            let rawBody = rawChar ']' <> body
                d = shallower depth
             in collectAfterValue early rawBody base d ctxs rest text'
          [] -> leave early (HQDecodeError ExpectedCommaOrEnd)
        _ -> leave early (HQDecodeError ExpectedCommaOrEnd)

-- | A value just completed: return at entry depth, else expect the
-- enclosing separator. Mirror of 'afterValue'.
collectAfterValue ::
  Early HQError ->
  RawBody ->
  NestDepth ->
  NestDepth ->
  [DecodeContext] ->
  Text ->
  StreamIO Text () ->
  IO Collected
collectAfterValue early body base depth stack rest text'
  | depth <= base = pure (Collected (SkipRest rest text') body)
  | otherwise = case stack of
      DecodeArray : _ ->
        collectExpect early body base depth stack ExpectArrComma rest text'
      DecodeObject : _ ->
        collectExpect early body base depth stack ExpectObjComma rest text'
      [] -> pure (Collected (SkipRest rest text') body)

-- | Structural skip loop; ends when the depth pops back to its entry level.
skipExpect ::
  Early HQError ->
  NestDepth ->
  NestDepth ->
  [DecodeContext] ->
  SkipExpect ->
  Text ->
  StreamIO Text () ->
  IO SkipRest
skipExpect early base depth stack ExpectValue input text = do
  (c, SkipRest rest text') <- nextSkipChar early input text
  case c of
    '{' ->
      let ctxts = DecodeObject : stack
       in skipExpect early base (deeper depth) ctxts ExpectKey rest text'
    '[' ->
      let ctxts = (DecodeArray : stack)
       in skipExpect early base (deeper depth) ctxts ExpectValue rest text'
    '"' -> do
      (SkipRest rest' text'') <- skipStringBody early rest text'
      afterValue early base depth stack rest' text''
    't' -> do
      (SkipRest rest' text'') <- skipKeywordBody early "true" 1 rest text'
      afterValue early base depth stack rest' text''
    'f' -> do
      (SkipRest rest' text'') <- skipKeywordBody early "false" 1 rest text'
      afterValue early base depth stack rest' text''
    'n' -> do
      (SkipRest rest' text'') <- skipKeywordBody early "null" 1 rest text'
      afterValue early base depth stack rest' text''
    _
      | c == '-' || isDigit c -> do
          (SkipRest rest' text'') <- skipNumberBody early c rest text'
          afterValue early base depth stack rest' text''
      | c == ']' -> case stack of
          DecodeArray : ctxs ->
            afterValue early base (shallower depth) ctxs rest text'
          _ -> leave early (HQDecodeError (UnexpectedChar c))
      | otherwise -> leave early (HQDecodeError (UnexpectedChar c))
skipExpect early base depth stack ExpectKey input text = do
  (c, SkipRest rest text') <- nextSkipChar early input text
  case c of
    '}' -> case stack of
      _ : ctxs -> afterValue early base (shallower depth) ctxs rest text'
      [] -> leave early (HQDecodeError ExpectedObjectKey)
    '"' -> do
      (SkipRest rest' text'') <- skipStringBody early rest text'
      skipExpect early base depth stack ExpectColon rest' text''
    _ -> leave early (HQDecodeError ExpectedObjectKey)
skipExpect early base depth stack ExpectColon input text = do
  (c, SkipRest rest text') <- nextSkipChar early input text
  case c of
    ':' -> skipExpect early base depth stack ExpectValue rest text'
    _ -> leave early (HQDecodeError ExpectedColon)
skipExpect early base depth stack ExpectObjComma input text = do
  (c, SkipRest rest text') <- nextSkipChar early input text
  case c of
    ',' -> skipExpect early base depth stack ExpectKey rest text'
    '}' -> case stack of
      _ : ctxs -> afterValue early base (shallower depth) ctxs rest text'
      [] -> leave early (HQDecodeError ExpectedCommaOrEnd)
    _ -> leave early (HQDecodeError ExpectedCommaOrEnd)
skipExpect early base depth stack ExpectArrComma input text = do
  (c, SkipRest rest text') <- nextSkipChar early input text
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
  IO SkipRest
afterValue early base depth stack rest text'
  | depth <= base = pure (SkipRest rest text')
  | otherwise = case stack of
      DecodeArray : _ ->
        skipExpect early base depth stack ExpectArrComma rest text'
      DecodeObject : _ ->
        skipExpect early base depth stack ExpectObjComma rest text'
      [] -> pure $ SkipRest rest text'

-- Validates escapes exactly like the decoder; bytes discarded, never captured.
skipStringBody :: Early HQError -> Text -> StreamIO Text () -> IO SkipRest
skipStringBody early = go
  where
    go !inp !txt = do
      let (_, rest) = T.span isStringChar inp
      case T.uncons rest of
        Nothing -> do
          (SkipRest chunk rest') <- pullSkipText early mempty txt
          go chunk rest'
        Just (c, rest')
          | c == '"' -> pure (SkipRest rest' txt)
          | c == '\\' -> skipEscape early rest' txt
          | otherwise -> leave early (HQDecodeError (UnexpectedChar c))

-- | Skip one escape sequence (input starts after the backslash).
skipEscape :: Early HQError -> Text -> StreamIO Text () -> IO SkipRest
skipEscape early !input !text = do
  (SkipRest chunk rest) <- pullSkipText early input text
  case T.uncons chunk of
    Nothing -> skipEscape early mempty rest
    Just (c, rest')
      | c == 'u' -> skipUnicode early rest' rest 0 0
      | isSimpleEscape c -> skipStringBody early rest' rest
      | otherwise -> leave early (HQDecodeError (InvalidEscape c))

-- | Skip a @\u@ escape's four hex digits.
skipUnicode ::
  Early HQError -> Text -> StreamIO Text () -> Int -> Int -> IO SkipRest
skipUnicode early !input !text !value !digits = do
  (SkipRest chunk rest0) <- pullSkipText early input text
  let (value', digits', rest') = accumulateHex value digits chunk
  if digits' >= 4
    then finishUnicode early rest' rest0 value'
    else
      if T.null rest'
        then skipUnicode early mempty rest0 value' digits'
        else leave early (HQDecodeError InvalidUnicodeEscape)

-- | Validate a completed @\u@ escape.
finishUnicode :: Early HQError -> Text -> StreamIO Text () -> Int -> IO SkipRest
finishUnicode early !input !text !value
  | isHighSurrogate value = skipLowSurrogate early input text
  | isLowSurrogate value = leave early (HQDecodeError InvalidSurrogatePair)
  | otherwise = skipStringBody early input text

-- | Skip a low-surrogate escape after a high surrogate.
skipLowSurrogate :: Early HQError -> Text -> StreamIO Text () -> IO SkipRest
skipLowSurrogate early !input !text = do
  (SkipRest chunk rest) <- pullSkipText early input text
  case T.uncons chunk of
    Nothing -> skipLowSurrogate early mempty rest
    Just ('\\', rest') -> case T.uncons rest' of
      Nothing -> skipLowBackslash early rest
      Just ('u', rest'') -> skipLowDigits early rest'' rest 0 0
      _ -> leave early (HQDecodeError InvalidSurrogatePair)
    _ -> leave early (HQDecodeError InvalidSurrogatePair)

-- | The low surrogate's backslash arrived at a chunk end.
skipLowBackslash :: Early HQError -> StreamIO Text () -> IO SkipRest
skipLowBackslash early !text = do
  (SkipRest chunk rest) <- pullSkipText early mempty text
  case T.uncons chunk of
    Nothing -> skipLowBackslash early rest
    Just ('u', rest') -> skipLowDigits early rest' rest 0 0
    _ -> leave early (HQDecodeError InvalidSurrogatePair)

-- | Skip the low surrogate's four hex digits.
skipLowDigits ::
  Early HQError -> Text -> StreamIO Text () -> Int -> Int -> IO SkipRest
skipLowDigits early !input !text !value !digits = do
  (SkipRest chunk rest0) <- pullSkipText early input text
  let (value', digits', rest') = accumulateHex value digits chunk
  if digits' >= 4
    then
      if isLowSurrogate value'
        then skipStringBody early rest' rest0
        else leave early $ HQDecodeError InvalidSurrogatePair
    else
      if T.null rest'
        then skipLowDigits early mempty rest0 value' digits'
        else leave early $ HQDecodeError InvalidUnicodeEscape

-- | Skip a number starting at its first character. Shares
-- 'collectNumber' and drops the body: one scanner for both modes.
skipNumberBody ::
  Early HQError -> Char -> Text -> StreamIO Text () -> IO SkipRest
skipNumberBody early c inp txt = do
  collectedRest <$> collectNumber early (rawChar c) phase inp txt
  where
    phase = fromMaybe NumberSign (numberPhaseFromFirstChar c)

-- | Skip a keyword whose first character was consumed. A complete
-- keyword at exhaustion succeeds, mirroring the decoder. Shares
-- 'scanKeyword' and drops the body.
skipKeywordBody ::
  Early HQError -> Text -> Int -> Text -> StreamIO Text () -> IO SkipRest
skipKeywordBody early keyword index input text = do
  collectedRest <$> scanKeyword early index input text keyword

-- | Skip a keyword, capturing its raw bytes. Mirrors 'skipKeywordBody';
-- the literal is fixed, so it is returned whole on success.
skipKeywordCollect ::
  Early HQError -> Text -> StreamIO Text () -> Text -> IO Collected
skipKeywordCollect early = scanKeyword early 1

-- | The shared keyword scanner both modes use: match the literal from
-- @index@, then require a JSON delimiter. The literal is fixed, so
-- the body is returned whole on success.
scanKeyword ::
  Early HQError -> Int -> Text -> StreamIO Text () -> Text -> IO Collected
scanKeyword early ix0 inp0 txt0 keyword = go ix0 inp0 txt0
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
            Left () ->
              let rawBody = RawBody [keyword] (T.length keyword)
               in pure $ Collected (SkipRest mempty txt) rawBody
            Right (chunk, rest) -> checkDelim chunk rest
      | otherwise = case T.uncons inp of
          Nothing -> checkDelim mempty txt
          Just (c, _)
            | isJsonDelimiter c ->
                let rawBody = RawBody [keyword] (T.length keyword)
                 in pure $ Collected (SkipRest inp txt) rawBody
            | otherwise -> leave early (HQDecodeError (InvalidKeyword keyword))

-- | Skip a number, capturing its raw bytes.
skipNumberCollect :: Early HQError -> Text -> StreamIO Text () -> IO Collected
skipNumberCollect early input text = case T.uncons input of
  Nothing -> do
    (SkipRest chunk rest) <- pullSkipText early mempty text
    skipNumberCollect early chunk rest
  Just (c, rest) ->
    let phase = fromMaybe NumberSign (numberPhaseFromFirstChar c)
     in collectNumber early (rawChar c) phase rest text

collectNumber ::
  Early HQError ->
  RawBody ->
  NumberPhase ->
  Text ->
  StreamIO Text () ->
  IO Collected
collectNumber early body phase inp txt
  | T.null inp = do
      result <- S.next txt
      case result of
        Left ()
          | isValidNumberFinal phase -> pure (Collected (SkipRest mempty txt) body)
          | otherwise ->
              leave early $ HQDecodeError $ InvalidNumber (T.concat (reverse (rawFrags body)))
        Right (chunk, rest) -> collectNumber early body phase chunk rest
  | otherwise = loop body phase 0
  where
    len = T.length inp
    loop b p pos
      | pos >= len = collectNumber early (RawBody (inp : rawFrags b) (rawSize b + len)) p mempty txt
      | acceptsDigitRun p = runDigits b p pos
      | otherwise = stepChar b p pos
    -- Digits self-loop in these phases: consume the maximal run with
    -- one span, then dispatch once on the terminating character.
    runDigits b' p' pos' =
      let pos'' = pos' + T.length (T.takeWhile isDigit (T.drop pos' inp))
       in if pos'' >= len
            then collectNumber early (RawBody (inp : rawFrags b') (rawSize b' + len)) p' mempty txt
            else stepChar b' p' pos''
    stepChar b' p' pos' =
      let c = T.index inp pos'
          piece = T.take pos' inp
          body' = RawBody (piece : rawFrags b') (rawSize b' + pos')
       in case advanceNumber p' c of
            NumberEnd
              | isValidNumberFinal p' -> pure (Collected (SkipRest (T.drop pos' inp) txt) body')
              | otherwise ->
                  leave early
                    $ HQDecodeError
                    $ InvalidNumber (T.concat (reverse (rawFrags b') <> [piece]))
            NumberError ->
              leave early
                $ HQDecodeError
                $ InvalidNumber (T.concat (reverse (rawFrags b') <> [piece]) <> T.singleton c)
            NumberStep p'' -> loop b' p'' (pos' + 1)

-- | Skip a string, capturing its raw bytes, quotes included. The
-- opening quote seeds the body (oldest position); the closing quote
-- is consed at the end.
skipStringCollect :: Early HQError -> Text -> StreamIO Text () -> IO Collected
skipStringCollect early = collectStringGo early (rawChar '"')

collectStringGo ::
  Early HQError -> RawBody -> Text -> StreamIO Text () -> IO Collected
collectStringGo early !body !inp !txt = do
  let (safe, rest) = T.span isStringChar inp
      body' = RawBody (safe : rawFrags body) (rawSize body + T.length safe)
  case T.uncons rest of
    Nothing -> do
      (SkipRest chunk rest') <- pullSkipText early mempty txt
      collectStringGo early body' chunk rest'
    Just (c, rest')
      | c == '"' -> pure (Collected (SkipRest rest' txt) (rawChar '"' <> body'))
      | c == '\\' -> collectEscape early body' rest' txt
      | otherwise -> leave early (HQDecodeError (UnexpectedChar c))

-- | Capture one escape sequence, prepending its raw bytes.
collectEscape ::
  Early HQError -> RawBody -> Text -> StreamIO Text () -> IO Collected
collectEscape early !body !input !text = do
  (SkipRest chunk rest) <- pullSkipText early input text
  case T.uncons chunk of
    Nothing -> collectEscape early body mempty rest
    Just (c, rest')
      | c == 'u' -> collectUnicode early (esc 'u') rest' rest 0 0
      | isSimpleEscape c -> collectStringGo early (esc c) rest' rest
      | otherwise -> leave early (HQDecodeError (InvalidEscape c))
  where
    esc c = RawBody (T.pack ['\\', c] : rawFrags body) (rawSize body + 2)

-- | Capture a @\u@ escape's four hex digits. Single hex path via 'accumulateHex'.
collectUnicode ::
  Early HQError ->
  RawBody ->
  Text ->
  StreamIO Text () ->
  Int ->
  Int ->
  IO Collected
collectUnicode early !body !input !text !value !digits = do
  (SkipRest chunk rest0) <- pullSkipText early input text
  let (value', digits', rest') = accumulateHex value digits chunk
      piece = T.take (T.length chunk - T.length rest') chunk
      body' = RawBody (piece : rawFrags body) (rawSize body + T.length piece)
  if digits' >= 4
    then finishUnicodeCollect early body' rest' rest0 value'
    else
      if T.null rest'
        then collectUnicode early body' mempty rest0 value' digits'
        else leave early (HQDecodeError InvalidUnicodeEscape)

-- | Validate a completed @\u@ escape.
finishUnicodeCollect ::
  Early HQError -> RawBody -> Text -> StreamIO Text () -> Int -> IO Collected
finishUnicodeCollect early !body !input !text !value
  | isHighSurrogate value = collectLowSurrogate early body input text value
  | isLowSurrogate value = leave early (HQDecodeError InvalidSurrogatePair)
  | otherwise = collectStringGo early body input text

-- | Capture a low-surrogate escape after a high surrogate.
collectLowSurrogate ::
  Early HQError -> RawBody -> Text -> StreamIO Text () -> Int -> IO Collected
collectLowSurrogate early !body !input !text !high = do
  (SkipRest chunk rest) <- pullSkipText early input text
  case T.uncons chunk of
    Nothing -> collectLowSurrogate early body mempty rest high
    Just ('\\', rest') -> case T.uncons rest' of
      Nothing -> collectLowSurrogateBackslash early (rawChar '\\' <> body) rest high
      Just ('u', rest'') ->
        collectLowDigits early (RawBody (T.pack ['\\', 'u'] : rawFrags body) (rawSize body + 2)) rest'' rest high 0 0
      _ -> leave early (HQDecodeError InvalidSurrogatePair)
    _ -> leave early (HQDecodeError InvalidSurrogatePair)

-- | The low surrogate's backslash arrived at a chunk end.
collectLowSurrogateBackslash ::
  Early HQError -> RawBody -> StreamIO Text () -> Int -> IO Collected
collectLowSurrogateBackslash early !body !text !high = do
  (SkipRest chunk rest) <- pullSkipText early mempty text
  case T.uncons chunk of
    Nothing -> collectLowSurrogateBackslash early body rest high
    Just ('u', rest') -> collectLowDigits early (rawChar 'u' <> body) rest' rest high 0 0
    _ -> leave early (HQDecodeError InvalidSurrogatePair)

-- | Capture the low surrogate's four hex digits.
collectLowDigits ::
  Early HQError ->
  RawBody ->
  Text ->
  StreamIO Text () ->
  Int ->
  Int ->
  Int ->
  IO Collected
collectLowDigits early !body !input !text !high !value !digits
  | digits == 4 =
      if isLowSurrogate value
        then collectStringGo early body input text
        else leave early $ HQDecodeError InvalidSurrogatePair
  | otherwise = do
      (SkipRest chunk rest) <- pullSkipText early input text
      case T.uncons chunk of
        Nothing -> collectLowDigits early body mempty rest high value digits
        Just _ -> do
          let (value', digits', rest') = accumulateHex value digits chunk
              consumed = digits' - digits
              piece = T.take consumed chunk
              body' = RawBody (piece : rawFrags body) (rawSize body + consumed)
          if digits' == 4
            then
              if isLowSurrogate value'
                then collectStringGo early body' rest' rest
                else leave early $ HQDecodeError InvalidSurrogatePair
            else
              if T.null rest'
                then collectLowDigits early body' mempty rest high value' digits'
                else leave early $ HQDecodeError InvalidUnicodeEscape
