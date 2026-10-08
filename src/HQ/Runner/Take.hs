{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner.Take where

import Data.Char (isDigit)
import Data.Text qualified as T
import HQ.Early (Early, leave)
import HQ.JSON.Decoder
import HQ.JSON.Decoder qualified as Decoder
import HQ.JSON.Decoder.Skip (skipContainerCollect, skipNumberCollect, skipStringCollect)
import HQ.JSON.Encoder
import HQ.JSON.Event (JSONEvent (..), matchingClose)
import HQ.Runner.Cursor
import HQ.Runner.Error (RunnerError (..))
import Relude hiding (Compose, id, many, some, state)
import Streaming.Prelude qualified as S

-- | Pull a single event, with the advanced cursor.
pullOne :: Early HQError -> Cursor -> IO (JSONEvent, Cursor)
pullOne early =
  pullCursor early
    >=> maybe (leave early $ HQRunnerError UnexpectedEndOfInput) pure

-- | True when the decoder still holds pending input that must be
-- stepped before pulling more text. Single name for the 4x
-- @not (T.null (decoderInput ...))@ guard.
hasPending :: DecoderState -> Bool
hasPending dec = not (T.null (decoderInput dec))

-- | Shared 'finish' decision for take loops: top-level exhaustion
-- means a truncated take here (contrast 'atEnd'/'finishEnd', where it
-- ends cleanly). Decided by 'classifyTerminal', like both drivers.
finishTakeEvent :: Early HQError -> DecoderState -> IO (JSONEvent, DecoderState)
finishTakeEvent early dec = case classifyTerminal dec of
  TerminalEnd -> leave early $ HQRunnerError UnexpectedEndOfInput
  TerminalError err -> leave early (HQDecodeError err)
  TerminalEmit event dec' -> pure (event, dec')

-- | Stream the events of exactly one complete JSON value. Lazy: stops
-- pulling once the value is complete.
takeValue :: Early HQError -> Continuation
takeValue early cursor = do
  (event, cursor') <- lift (pullOne early cursor)
  S.yield event
  case matchingClose event of
    Just closing -> takeContainerFrom early closing cursor'
    Nothing -> case event of
      JSONEndArray -> lift (leave early $ HQRunnerError UnexpectedEndOfArray)
      JSONEndObject -> lift (leave early $ HQRunnerError UnexpectedEndOfObject)
      JSONObjectKey _ -> lift (leave early $ HQRunnerError UnexpectedObjectKey)
      _ -> pure cursor'

takeFirstValue :: EventStream r -> EventStream ()
takeFirstValue = go (0 :: Int)
  where
    go depth stream = do
      result <- lift (S.next stream)
      case result of
        Left _ -> pure ()
        Right (event, rest) -> do
          S.yield event
          case event of
            JSONBeginArray -> go (depth + 1) rest
            JSONBeginObject -> go (depth + 1) rest
            JSONEndArray
              | depth == 1 -> pure ()
              | otherwise -> go (depth - 1) rest
            JSONEndObject
              | depth == 1 -> pure ()
              | otherwise -> go (depth - 1) rest
            _
              | depth == 0 -> pure ()
              | otherwise -> go depth rest

-- | Take a container body event by event, driving the decoder directly.
takeContainerFrom :: Early HQError -> JSONEvent -> Cursor -> EventStream Cursor
takeContainerFrom early closing = go
  where
    go :: Cursor -> EventStream Cursor
    go (Cursor buf dec txt) = case buf of
      event : rest -> emit event $ Cursor rest dec txt
      [] -> stepMore dec txt
    -- Step while the decoder holds pending input: mid-value pauses
    -- (e.g. a high surrogate awaiting its low half) must be stepped,
    -- not finished. Only a truly drained decoder may pull more text.
    -- Mirrors 'pullMore' in "HQ.Runner.Cursor".
    stepMore :: DecoderState -> StreamIO Text () -> EventStream Cursor
    stepMore dec txt = case Decoder.step dec of
      Left err -> lift (leave early $ HQDecodeError err)
      Right (Emit event dec') -> emit event $ Cursor [] dec' txt
      Right (NeedInput dec')
        | hasPending dec' -> stepMore dec' txt
        | otherwise -> pullMore dec' txt
      Right (Done _) -> lift (leave early $ HQRunnerError UnexpectedEndOfInput)
    emit event cursor = do
      S.yield event
      if event == closing
        then pure cursor
        else case matchingClose event of
          Just end -> nested end cursor
          Nothing -> go cursor
    nested end cursor = takeContainerFrom early end cursor >>= go
    pullMore dec txt = do
      result <- lift (S.next txt)
      case result of
        Left () -> finishTake dec
        Right (chunk, rest) -> case Decoder.feed chunk dec of
          Left err -> lift (leave early (HQDecodeError err))
          Right (Emit event dec') -> emit event $ Cursor [] dec' rest
          Right (NeedInput dec') -> stepMore dec' rest
          Right (Done _) -> lift (leave early $ HQRunnerError UnexpectedEndOfInput)
    -- Mirror terminal 'finish' policy: a value completed exactly at end
    -- of input still yields its final event; anything else ends the
    -- take the same way the event-stream takes did.
    finishTake dec = do
      (event, dec') <- lift (finishTakeEvent early dec)
      emit event $ Cursor [] dec' (pure ())

emitChunkBuffer :: Builder -> Int -> EncoderState -> ChunkStream IO EncoderState
emitChunkBuffer b s (EncoderState ctxs depth pend pendSize) =
  let !pend' = pend <> b
      !pendSize' = pendSize + s
   in if pendSize' >= batchSize
        then do
          S.yield (Chunk pend' pendSize')
          pure (EncoderState ctxs depth mempty 0)
        else pure (EncoderState ctxs depth pend' pendSize')

flushEncoderChunks :: EncoderState -> ChunkStream IO EncoderState
flushEncoderChunks st = case flushEncoderState st of
  (Just chunk, st') -> S.yield chunk >> pure st'
  (Nothing, st') -> pure st'

emitChunk ::
  EncoderConfig ->
  JSONEvent ->
  EncoderState ->
  ChunkStream IO EncoderState
emitChunk config event st = case formatEvent config st event of
  (Chunk b s, st') -> emitChunkBuffer b s st'

--------------------------------------------------------------------------------
-- Shared chunk walk combinators (single implementation for Rewrite)
--------------------------------------------------------------------------------

-- | Pull one event; on end return unchanged, otherwise run the handler.
-- Chunk-stream analogue of 'HQ.Runner.Cursor.onEventOrEnd'.
onEventOrEndChunks ::
  Early HQError ->
  Cursor ->
  EncoderState ->
  ((JSONEvent, Cursor) -> ChunkStream IO (EncoderState, Cursor)) ->
  ChunkStream IO (EncoderState, Cursor)
onEventOrEndChunks early input st f = do
  result <- lift (pullCursor early input)
  case result of
    Nothing -> pure (st, input)
    Just pair -> f pair

-- | Match the next event against a table of opens; anything else is
-- passed through as a whole value. Chunk-stream analogue of
-- 'HQ.Runner.Cursor.onOpenOrSkip'. Inlined so the table specializes
-- per call site.
{-# INLINE onOpenOrSkipChunks #-}
onOpenOrSkipChunks ::
  Early HQError ->
  EncoderConfig ->
  Cursor ->
  EncoderState ->
  [(JSONEvent, Cursor -> EncoderState -> ChunkStream IO (EncoderState, Cursor))] ->
  ChunkStream IO (EncoderState, Cursor)
onOpenOrSkipChunks early config input st arms =
  onEventOrEndChunks early input st $ \(event, rest) ->
    case [handle | (open, handle) <- arms, open == event] of
      (handle : _) -> handle rest st
      [] -> takeValueChunks early config (pushCursor event rest) st

-- | Loop pulling member steps until the container ends, emitting
-- @close@; @body@ handles each step, threading the encoder state.
-- Shared by 'traverseObjectChunks' and 'traverseArrayChunks': the
-- close emission is the only difference. Inlined so @close@/@next@/
-- @body@ specialize per walk.
{-# INLINE walkMembersChunks #-}
walkMembersChunks ::
  JSONEvent ->
  EncoderConfig ->
  (Cursor -> IO (Either Cursor (a, Cursor))) ->
  ((a, Cursor) -> EncoderState -> ChunkStream IO (EncoderState, Cursor)) ->
  Cursor ->
  EncoderState ->
  ChunkStream IO (EncoderState, Cursor)
walkMembersChunks close config next body = go
  where
    go stream s = do
      step' <- lift (next stream)
      case step' of
        Left rest -> do
          s' <- emitChunk config close s
          pure (s', rest)
        Right pair -> do
          (s', after) <- body pair s
          go after s'

-- | Walk an object body, emitting @JSONEndObject@ at the end.
-- Caller must have emitted @JSONBeginObject@ already.
traverseObjectChunks ::
  Early HQError ->
  EncoderConfig ->
  Cursor ->
  EncoderState ->
  (Text -> Cursor -> EncoderState -> ChunkStream IO (EncoderState, Cursor)) ->
  ChunkStream IO (EncoderState, Cursor)
traverseObjectChunks early config input st body =
  walkMembersChunks JSONEndObject config (expectObjectStep early) (uncurry body) input st

-- | Walk an array body, emitting @JSONEndArray@ at the end.
traverseArrayChunks ::
  Early HQError ->
  EncoderConfig ->
  Cursor ->
  EncoderState ->
  ((JSONEvent, Cursor) -> EncoderState -> ChunkStream IO (EncoderState, Cursor)) ->
  ChunkStream IO (EncoderState, Cursor)
traverseArrayChunks early config input st body =
  walkMembersChunks JSONEndArray config (expectArrayStep early) body input st

-- | Emit a key then passthrough its value; the 4x pattern in
-- @rewriteMember@/pairs/allKeys@.
emitKeyAndTake ::
  Early HQError ->
  EncoderConfig ->
  Text ->
  Cursor ->
  EncoderState ->
  ChunkStream IO (EncoderState, Cursor)
emitKeyAndTake early config key rest s = do
  s' <- emitChunk config (JSONObjectKey key) s
  takeValueChunks early config rest s'

-- | Take one complete value, transcribing it straight to chunks.
-- Strings and numbers take a verbatim raw path ('peekRawScalar'),
-- and so do whole containers in compact mode ('takeRawContainerChunks'
-- below): chunk-side only. The event-side take ('takeValue') always
-- decodes, so both paths must agree on value boundaries; the raw
-- scanners ('skipStringCollect'/'skipNumberCollect'/
-- 'skipContainerCollect') mirror the decoder ('consumeString'/
-- 'stepNumber'/structural loop) exactly, and the split/adversarial
-- tests cover the agreement. Change one side only with a test on
-- the other.
takeValueChunks ::
  Early HQError ->
  EncoderConfig ->
  Cursor ->
  EncoderState ->
  ChunkStream IO (EncoderState, Cursor)
takeValueChunks early config cursor st = case peekRawScalar cursor of
  Just (RawString afterQuote, dec, txt) ->
    takeRawStringChunks early config afterQuote dec txt st
  Just (RawNumber atNumber, dec, txt) ->
    takeRawNumberChunks early config atNumber dec txt st
  Nothing -> do
    (event, cursor') <- lift (pullOne early cursor)
    let slowPath = do
          st' <- emitChunk config event st
          case matchingClose event of
            Just closing -> takeContainerChunks early config closing cursor' st'
            Nothing -> case event of
              JSONEndArray -> lift (leave early $ HQRunnerError UnexpectedEndOfArray)
              JSONEndObject -> lift (leave early $ HQRunnerError UnexpectedEndOfObject)
              JSONObjectKey _ -> lift (leave early $ HQRunnerError UnexpectedObjectKey)
              _ -> pure (st', cursor')
    -- Wholesale containers bypass event decoding only when nothing is
    -- buffered (buffered replay has no source text to collect) and the
    -- output is compact (verbatim bytes are already canonical there;
    -- pretty output must be generated).
    case event of
      JSONBeginArray -> case cursor' of
        Cursor [] dec txt
          | outputStyle config == Compact ->
              takeRawContainerChunks early config event dec txt st
        _ -> slowPath
      JSONBeginObject -> case cursor' of
        Cursor [] dec txt
          | outputStyle config == Compact ->
              takeRawContainerChunks early config event dec txt st
        _ -> slowPath
      _ -> slowPath

-- | A peeked string or number value: payload after the quote / at the
-- first digit, pending colon already skipped.
data RawScalar = RawString Text | RawNumber Text

-- | Peek a string/number value; decline anything else (including
-- buffered replay cursors) so existing paths report errors.
peekRawScalar :: Cursor -> Maybe (RawScalar, DecoderState, StreamIO Text ())
peekRawScalar cursor
  | null buf,
    Just start <- peekValue phase input,
    Just (c, rest) <- T.uncons start = case c of
      '"' -> Just (RawString rest, decoder, txt)
      '-' -> Just (RawNumber start, decoder, txt)
      _ | isDigit c -> Just (RawNumber start, decoder, txt)
      _ -> Nothing
  | otherwise = Nothing
  where
    Cursor buf decoder txt = cursor
    DecoderState {decoderInput = input, decoderPhase = phase} = decoder
    -- Member values hide behind their colon; anything else must already
    -- be in value position.
    peekValue DecoderPhaseValue inp = Just (T.dropWhile isWhitespace inp)
    peekValue DecoderPhaseObjectColon inp =
      case T.uncons (T.dropWhile isWhitespace inp) of
        Just (':', rest) -> Just (T.dropWhile isWhitespace rest)
        _ -> Nothing
    peekValue _ _ = Nothing

-- | Transcribe a peeked string value verbatim (escapes preserved).
takeRawStringChunks ::
  Early HQError ->
  EncoderConfig ->
  Text ->
  DecoderState ->
  StreamIO Text () ->
  EncoderState ->
  ChunkStream IO (EncoderState, Cursor)
takeRawStringChunks early config afterQuote dec txt st = do
  (remainder, rawB, rawS, rest) <- lift (skipStringCollect early afterQuote txt)
  let (Chunk b s, st1) = transcribeRawString config st rawB rawS
  st' <- emitChunkBuffer b s st1
  pure (st', Cursor [] (finishValue dec {decoderInput = remainder}) rest)

-- | Transcribe a peeked number value verbatim (no 'Scientific' roundtrip).
takeRawNumberChunks ::
  Early HQError ->
  EncoderConfig ->
  Text ->
  DecoderState ->
  StreamIO Text () ->
  EncoderState ->
  ChunkStream IO (EncoderState, Cursor)
takeRawNumberChunks early config atNumber dec txt st = do
  (remainder, rawB, rawS, rest) <- lift (skipNumberCollect early atNumber txt)
  let (Chunk b s, st1) = transcribeRawBytes config st rawB rawS
  st' <- emitChunkBuffer b s st1
  pure (st', Cursor [] (finishValue dec {decoderInput = remainder}) rest)

-- | Transcribe a whole container value verbatim in one chunk. The
-- cursor must be unbuffered with the decoder just past the opening
-- bracket (same precondition as walking it with 'takeContainerChunks';
-- the open event itself was already pulled). Encoder-state transitions
-- match the scalar raw path exactly (one 'transcribeRawBytes'),
-- since the open/close brackets travel inside the raw bytes.
takeRawContainerChunks ::
  Early HQError ->
  EncoderConfig ->
  JSONEvent ->
  DecoderState ->
  StreamIO Text () ->
  EncoderState ->
  ChunkStream IO (EncoderState, Cursor)
takeRawContainerChunks early config open dec txt st = do
  (dec', rawB, rawS, rest) <- lift (skipContainerCollect early open dec txt)
  let (Chunk b s, st1) = transcribeRawBytes config st rawB rawS
  st' <- emitChunkBuffer b s st1
  pure (st', Cursor [] dec' rest)

-- | Pending-builder flush threshold; chunk boundaries never change bytes.
batchSize :: Int
batchSize = 8192

-- | Take a container body, fusing decode and format per event with no
-- intermediate event stream.
takeContainerChunks ::
  Early HQError ->
  EncoderConfig ->
  JSONEvent ->
  Cursor ->
  EncoderState ->
  ChunkStream IO (EncoderState, Cursor)
takeContainerChunks early config closing = go
  where
    go :: Cursor -> EncoderState -> ChunkStream IO (EncoderState, Cursor)
    go cur st = case cur of
      Cursor (event : buffered) dec txt ->
        emit event (Cursor buffered dec txt) st
      Cursor [] _ _ ->
        stepMore cur st
    stepMore :: Cursor -> EncoderState -> ChunkStream IO (EncoderState, Cursor)
    stepMore (Cursor _ dec txt) st = case Decoder.step dec of
      Left err -> lift (leave early (HQDecodeError err))
      Right (Emit event dec') ->
        emit event (Cursor [] dec' txt) st
      Right (NeedInput dec')
        | hasPending dec' -> stepMore (Cursor [] dec' txt) st
        | otherwise -> pullMore (Cursor [] dec' txt) st
      Right (Done _) -> lift (leave early $ HQRunnerError UnexpectedEndOfInput)
    emit :: JSONEvent -> Cursor -> EncoderState -> ChunkStream IO (EncoderState, Cursor)
    emit !event cur st = do
      st' <- emitChunk config event st
      if event == closing
        then pure (st', cur)
        else case matchingClose event of
          Just end -> nested end cur st'
          Nothing -> go cur st'
    nested :: JSONEvent -> Cursor -> EncoderState -> ChunkStream IO (EncoderState, Cursor)
    nested !end cur st = do
      (st', cursor') <- takeContainerChunks early config end cur st
      go cursor' st'
    pullMore :: Cursor -> EncoderState -> ChunkStream IO (EncoderState, Cursor)
    pullMore cur@(Cursor _ dec txt) st = do
      result <- lift (S.next txt)
      case result of
        Left () -> finishTake cur st
        Right (chunk, rest) -> case Decoder.feed chunk dec of
          Left err -> lift (leave early (HQDecodeError err))
          Right (Emit event dec') ->
            emit event (Cursor [] dec' rest) st
          Right (NeedInput dec') -> stepMore (Cursor [] dec' rest) st
          Right (Done _) -> lift (leave early $ HQRunnerError UnexpectedEndOfInput)
    -- Mirror terminal 'finish' policy, emitting the final event as a chunk.
    finishTake :: Cursor -> EncoderState -> ChunkStream IO (EncoderState, Cursor)
    finishTake (Cursor _ dec _) st = do
      (event, dec') <- lift (finishTakeEvent early dec)
      let newCursor = Cursor [] dec' (pure ())
       in emit event newCursor st
