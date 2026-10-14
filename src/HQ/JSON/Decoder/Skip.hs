{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Text-level value skipping: validate like the decoder but materialize nothing.
module HQ.JSON.Decoder.Skip
  ( skipContainerText,
    skipMemberValueText,
    skipStringCollect,
    skipNumberCollect,
    collectExpect,
    skipExpect,
    finishContainer,
    SkipExpect (..),
    SkipRest (..),
    Collected (..),
    CollectPaused (..),
    RawBody (..),
    RawSink (..),
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

-- | Output of the structural/string scanners: either discarded ('()')
-- or accumulated ('RawBody'). The two instances specialize away, so the
-- discard path compiles to the old hand-written skip code (no fragment
-- allocation) while both modes share one scanner. See SPECIALIZE pragmas
-- below.
class RawSink s where
  sinkChar :: Char -> s -> s
  sinkText :: Text -> s -> s
  sinkAppend :: s -> s -> s
  sinkEmbed :: RawBody -> s

  -- | Budget check at the top of 'generalExpect': a no-op for the
  -- discard path, a bounded-segment pause for the collect path.
  sinkCheck ::
    s ->
    SkipExpect ->
    NestDepth ->
    NestDepth ->
    [DecodeContext] ->
    Text ->
    StreamIO Text () ->
    IO ()

instance RawSink () where
  sinkChar _ _ = ()
  sinkText _ _ = ()
  sinkAppend _ _ = ()
  sinkEmbed _ = ()
  sinkCheck _ _ _ _ _ _ _ = pure ()

instance RawSink RawBody where
  sinkChar c body = rawChar c <> body
  sinkText t body
    | T.null t = body
    | otherwise = RawBody (t : rawFrags body) (rawSize body + T.length t)
  sinkAppend = (<>)
  sinkEmbed = identity
  sinkCheck body expect base depth stack input text
    | rawSize body > collectBudget =
        throwIO (CollectPaused body expect base depth stack input text)
    | otherwise = pure ()

data SkipExpect
  = ExpectValue
  | ExpectKey
  | ExpectColon
  | ExpectObjComma
  | ExpectArrComma

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
  showsPrec _ _ =
    showString "CollectPaused: raw container segment ready, resuming collect"

instance Exception CollectPaused

-- | Pull the next chunk, skipping empties. Exhaustion here is
-- 'UnexpectedEnd'.
pullSkipText :: Early HQError -> Text -> StreamIO Text () -> IO SkipRest
pullSkipText early !input !text
  | T.null input =
      S.next text >>= \case
        Left () -> leave early (HQDecodeError UnexpectedEnd)
        Right (chunk, rest) -> pullSkipText early chunk rest
  | otherwise = pure (SkipRest input text)

-- | The next non-whitespace character, pulling more chunks as needed.
nextSkipChar :: Early HQError -> Text -> StreamIO Text () -> IO (Char, SkipRest)
nextSkipChar early input text = case T.uncons (T.dropWhile isWhitespace input) of
  Just (c, rest) -> pure (c, SkipRest rest text)
  Nothing ->
    S.next text >>= \case
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
skipContainerText early open decoder@DecoderState {..} text = case open of
  JSONBeginArray -> skipRest ExpectValue
  JSONBeginObject -> skipRest ExpectKey
  _ -> pure (decoder, text)
  where
    -- The opener is already consumed, so the skip ends when the depth
    -- pops back below its entry level.
    skipRest expect = do
      let depth = decoderNestDepth
          base = shallower depth
      (SkipRest remainder rest) <-
        skipExpect early base depth decoderStack expect decoderInput text
      case decoderStack of
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

-- | Structural loop shared by skipping (discard, '()') and collecting
-- ('RawBody'): same states, same errors, same termination; only the
-- accumulator differs. Thin wrappers below specialize it, so both call
-- sites keep their old signatures and performance.
{-# INLINE generalExpect #-}
generalExpect ::
  (RawSink s) =>
  Early HQError ->
  s ->
  NestDepth ->
  NestDepth ->
  [DecodeContext] ->
  SkipExpect ->
  Text ->
  StreamIO Text () ->
  IO (SkipRest, s)
generalExpect early body base depth stack expect input text = do
  sinkCheck body expect base depth stack input text
  case expect of
    ExpectValue -> do
      (c, SkipRest rest text') <- nextSkipChar early input text
      case c of
        '{' ->
          generalExpect early (sinkChar '{' body) base (deeper depth) (DecodeObject : stack) ExpectKey rest text'
        '[' ->
          generalExpect early (sinkChar '[' body) base (deeper depth) (DecodeArray : stack) ExpectValue rest text'
        '"' -> do
          (SkipRest rest' text'', body') <-
            generalStringGo early (sinkChar '"' body) rest text'
          generalAfterValue early body' base depth stack rest' text''
        't' -> scalarKeyword "true" rest text'
        'f' -> scalarKeyword "false" rest text'
        'n' -> scalarKeyword "null" rest text'
        _
          | c == '-' || isDigit c -> do
              let phase = fromMaybe NumberSign (numberPhaseFromFirstChar c)
              (Collected (SkipRest rest' text'') nbody) <-
                collectNumber early (rawChar c) phase rest text'
              generalAfterValue early (sinkAppend (sinkEmbed nbody) body) base depth stack rest' text''
          | c == ']' -> case stack of
              DecodeArray : ctxs ->
                generalAfterValue early (sinkChar ']' body) base (shallower depth) ctxs rest text'
              _ -> leave early (HQDecodeError (UnexpectedChar c))
          | otherwise -> leave early (HQDecodeError (UnexpectedChar c))
      where
        scalarKeyword lit r t = do
          (Collected (SkipRest rest' text'') kbody) <-
            scanKeyword early 1 r t lit
          generalAfterValue early (sinkAppend (sinkEmbed kbody) body) base depth stack rest' text''
    ExpectKey -> do
      (c, SkipRest rest text') <- nextSkipChar early input text
      case c of
        '}' -> case stack of
          _ : ctxs ->
            generalAfterValue early (sinkChar '}' body) base (shallower depth) ctxs rest text'
          [] -> leave early (HQDecodeError ExpectedObjectKey)
        '"' -> do
          (SkipRest rest' text'', body') <- generalStringGo early (sinkChar '"' body) rest text'
          generalExpect early body' base depth stack ExpectColon rest' text''
        _ -> leave early (HQDecodeError ExpectedObjectKey)
    ExpectColon -> do
      (c, SkipRest rest text') <- nextSkipChar early input text
      case c of
        ':' -> generalExpect early (sinkChar ':' body) base depth stack ExpectValue rest text'
        _ -> leave early (HQDecodeError ExpectedColon)
    ExpectObjComma -> do
      (c, SkipRest rest text') <- nextSkipChar early input text
      case c of
        ',' -> generalExpect early (sinkChar ',' body) base depth stack ExpectKey rest text'
        '}' -> case stack of
          _ : ctxs ->
            generalAfterValue early (sinkChar '}' body) base (shallower depth) ctxs rest text'
          [] -> leave early (HQDecodeError ExpectedCommaOrEnd)
        _ -> leave early (HQDecodeError ExpectedCommaOrEnd)
    ExpectArrComma -> do
      (c, SkipRest rest text') <- nextSkipChar early input text
      case c of
        ',' -> generalExpect early (sinkChar ',' body) base depth stack ExpectValue rest text'
        ']' -> case stack of
          _ : ctxs ->
            generalAfterValue early (sinkChar ']' body) base (shallower depth) ctxs rest text'
          [] -> leave early (HQDecodeError ExpectedCommaOrEnd)
        _ -> leave early (HQDecodeError ExpectedCommaOrEnd)

{-# INLINE generalAfterValue #-}
generalAfterValue ::
  (RawSink s) =>
  Early HQError ->
  s ->
  NestDepth ->
  NestDepth ->
  [DecodeContext] ->
  Text ->
  StreamIO Text () ->
  IO (SkipRest, s)
generalAfterValue early body base depth stack rest text'
  | depth <= base = pure (SkipRest rest text', body)
  | otherwise = case stack of
      DecodeArray : _ ->
        generalExpect early body base depth stack ExpectArrComma rest text'
      DecodeObject : _ ->
        generalExpect early body base depth stack ExpectObjComma rest text'
      [] -> pure (SkipRest rest text', body)

-- | Structural collect loop; ends when the depth pops back to its
-- entry level. Specialization of 'generalExpect'.
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
collectExpect early body base depth stack expect input text = do
  (rest, body') <- generalExpect early body base depth stack expect input text
  pure (Collected rest body')
{-# INLINE collectExpect #-}

-- | Structural skip loop; ends when the depth pops back to its entry level.
-- Specialization of 'generalExpect'.
skipExpect ::
  Early HQError ->
  NestDepth ->
  NestDepth ->
  [DecodeContext] ->
  SkipExpect ->
  Text ->
  StreamIO Text () ->
  IO SkipRest
skipExpect early base depth stack expect input text = do
  (rest, _) <- generalExpect early () base depth stack expect input text
  pure rest
{-# INLINE skipExpect #-}

{-# SPECIALIZE generalExpect :: Early HQError -> () -> NestDepth -> NestDepth -> [DecodeContext] -> SkipExpect -> Text -> StreamIO Text () -> IO (SkipRest, ()) #-}
{-# SPECIALIZE generalExpect :: Early HQError -> RawBody -> NestDepth -> NestDepth -> [DecodeContext] -> SkipExpect -> Text -> StreamIO Text () -> IO (SkipRest, RawBody) #-}

{-# SPECIALIZE generalAfterValue :: Early HQError -> () -> NestDepth -> NestDepth -> [DecodeContext] -> Text -> StreamIO Text () -> IO (SkipRest, ()) #-}
{-# SPECIALIZE generalAfterValue :: Early HQError -> RawBody -> NestDepth -> NestDepth -> [DecodeContext] -> Text -> StreamIO Text () -> IO (SkipRest, RawBody) #-}

-- Single string scanner shared by both modes: validates escapes exactly
-- like the decoder; '()' discards bytes, 'RawBody' captures them
-- (quotes included). Specialized below so the skip path allocates nothing.
{-# INLINE generalStringGo #-}
generalStringGo ::
  (RawSink s) => Early HQError -> s -> Text -> StreamIO Text () -> IO (SkipRest, s)
generalStringGo early !body !inp !txt = do
  let (safe, rest) = T.span isStringChar inp
      body' = sinkText safe body
  case T.uncons rest of
    Nothing -> do
      (SkipRest chunk rest') <- pullSkipText early mempty txt
      generalStringGo early body' chunk rest'
    Just (c, rest')
      | c == '"' -> pure (SkipRest rest' txt, sinkChar '"' body')
      | c == '\\' -> generalEscape early body' rest' txt
      | otherwise -> leave early (HQDecodeError (UnexpectedChar c))

{-# INLINE generalEscape #-}
generalEscape ::
  (RawSink s) => Early HQError -> s -> Text -> StreamIO Text () -> IO (SkipRest, s)
generalEscape early !body !input !text = do
  (SkipRest chunk rest) <- pullSkipText early input text
  case T.uncons chunk of
    Nothing -> generalEscape early body mempty rest
    Just (c, rest')
      | c == 'u' -> generalUnicode early (sinkText (T.pack ['\\', 'u']) body) rest' rest 0 0
      | isSimpleEscape c -> generalStringGo early (sinkText (T.pack ['\\', c]) body) rest' rest
      | otherwise -> leave early (HQDecodeError (InvalidEscape c))

{-# INLINE generalUnicode #-}
generalUnicode ::
  (RawSink s) => Early HQError -> s -> Text -> StreamIO Text () -> Int -> Int -> IO (SkipRest, s)
generalUnicode early !body !input !text !value !digits = do
  (SkipRest chunk rest0) <- pullSkipText early input text
  let (value', digits', rest') = accumulateHex value digits chunk
      piece = T.take (T.length chunk - T.length rest') chunk
      body' = sinkText piece body
  if digits' >= 4
    then generalFinishUnicode early body' rest' rest0 value'
    else
      if T.null rest'
        then generalUnicode early body' mempty rest0 value' digits'
        else leave early (HQDecodeError InvalidUnicodeEscape)

{-# INLINE generalFinishUnicode #-}
generalFinishUnicode ::
  (RawSink s) => Early HQError -> s -> Text -> StreamIO Text () -> Int -> IO (SkipRest, s)
generalFinishUnicode early !body !input !text !value
  | isHighSurrogate value = generalLowSurrogate early body input text
  | isLowSurrogate value = leave early (HQDecodeError InvalidSurrogatePair)
  | otherwise = generalStringGo early body input text

{-# INLINE generalLowSurrogate #-}
generalLowSurrogate ::
  (RawSink s) => Early HQError -> s -> Text -> StreamIO Text () -> IO (SkipRest, s)
generalLowSurrogate early !body !input !text = do
  (SkipRest chunk rest) <- pullSkipText early input text
  case T.uncons chunk of
    Nothing -> generalLowSurrogate early body mempty rest
    Just ('\\', rest') -> case T.uncons rest' of
      Nothing -> generalLowBackslash early (sinkChar '\\' body) rest
      Just ('u', rest'') ->
        generalLowDigits early (sinkText (T.pack ['\\', 'u']) body) rest'' rest 0 0
      _ -> leave early (HQDecodeError InvalidSurrogatePair)
    _ -> leave early (HQDecodeError InvalidSurrogatePair)

{-# INLINE generalLowBackslash #-}
generalLowBackslash ::
  (RawSink s) => Early HQError -> s -> StreamIO Text () -> IO (SkipRest, s)
generalLowBackslash early !body !text = do
  (SkipRest chunk rest) <- pullSkipText early mempty text
  case T.uncons chunk of
    Nothing -> generalLowBackslash early body rest
    Just ('u', rest') -> generalLowDigits early (sinkChar 'u' body) rest' rest 0 0
    _ -> leave early (HQDecodeError InvalidSurrogatePair)

{-# INLINE generalLowDigits #-}
generalLowDigits ::
  (RawSink s) => Early HQError -> s -> Text -> StreamIO Text () -> Int -> Int -> IO (SkipRest, s)
generalLowDigits early !body !input !text !value !digits
  | digits == 4 =
      if isLowSurrogate value
        then generalStringGo early body input text
        else leave early $ HQDecodeError InvalidSurrogatePair
  | otherwise = do
      (SkipRest chunk rest) <- pullSkipText early input text
      case T.uncons chunk of
        Nothing -> generalLowDigits early body mempty rest value digits
        Just _ -> do
          let (value', digits', rest') = accumulateHex value digits chunk
              consumed = digits' - digits
              piece = T.take consumed chunk
              body' = sinkText piece body
          if digits' == 4
            then
              if isLowSurrogate value'
                then generalStringGo early body' rest' rest
                else leave early $ HQDecodeError InvalidSurrogatePair
            else
              if T.null rest'
                then generalLowDigits early body' mempty rest value' digits'
                else leave early $ HQDecodeError InvalidUnicodeEscape

{-# SPECIALIZE generalStringGo :: Early HQError -> () -> Text -> StreamIO Text () -> IO (SkipRest, ()) #-}
{-# SPECIALIZE generalStringGo :: Early HQError -> RawBody -> Text -> StreamIO Text () -> IO (SkipRest, RawBody) #-}

{-# SPECIALIZE generalEscape :: Early HQError -> () -> Text -> StreamIO Text () -> IO (SkipRest, ()) #-}
{-# SPECIALIZE generalEscape :: Early HQError -> RawBody -> Text -> StreamIO Text () -> IO (SkipRest, RawBody) #-}

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
skipStringCollect early inp txt = do
  (SkipRest rest txt', body) <- generalStringGo early (rawChar '"') inp txt
  pure (Collected (SkipRest rest txt') body)
{-# INLINE skipStringCollect #-}
