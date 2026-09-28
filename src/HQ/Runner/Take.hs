{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner.Take where

import Control.Monad.Error.Class (MonadError (throwError))
import qualified Data.Text as T
import HQ.Error (HQError (..))
import HQ.JSON.Decoder (DecodeError (..), DecoderPhase (..), DecoderResult (..), DecoderState (..), Next (..), StreamIO, finish, pullEvent)
import qualified HQ.JSON.Decoder as Decoder
import HQ.JSON.Encoder (Builder, Chunk (..), ChunkStream, EncoderConfig, EncoderState, formatEvent)
import HQ.JSON.Event (JSONEvent (..))
import HQ.Runner.Cursor (Continuation, Cursor (..), EventStream)
import HQ.Runner.Error (RunnerError (..))
import Relude hiding (Compose, id, many, some, state)
import qualified Streaming.Prelude as S

-- | Pull a single event for bulk takes, with the advanced cursor.
-- Used once per taken value (not per event).
pullOne :: Cursor -> ExceptT HQError IO (JSONEvent, Cursor)
pullOne (Cursor (event : buffered) decoder text) = pure (event, Cursor buffered decoder text)
pullOne (Cursor [] decoder text) = do
  pulled <- pullEvent decoder text
  case pulled of
    EndOfInput -> throwError $ HQRunnerError UnexpectedEndOfInput
    NextEvent event decoder' rest -> pure (event, Cursor [] decoder' rest)

-- | Stream the events of exactly one complete JSON value, returning the
-- cursor positioned immediately after it. Lazy: a consumer that stops
-- early (e.g. @S.take 1@) pulls only what it needs.
takeValue :: Continuation
takeValue cursor = do
  (event, cursor') <- lift (pullOne cursor)
  S.yield event
  case event of
    JSONBeginArray -> takeContainerFrom JSONEndArray cursor'
    JSONBeginObject -> takeContainerFrom JSONEndObject cursor'
    JSONEndArray -> throwError $ HQRunnerError UnexpectedEndOfArray
    JSONEndObject -> throwError $ HQRunnerError UnexpectedEndOfObject
    JSONObjectKey _ -> throwError $ HQRunnerError UnexpectedObjectKey
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

-- | Take a container body event by event, driving the decoder
-- directly with no tuples or wrappers on the hot path: the event,
-- buffer, decoder and text flow as explicit arguments.
takeContainerFrom :: JSONEvent -> Cursor -> EventStream Cursor
takeContainerFrom closing = go
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
      Left err -> throwError $ HQDecodeError err
      Right (Emit event dec') -> emit event $ Cursor [] dec' txt
      Right (NeedInput dec')
        | not (T.null (decoderInput dec')) -> stepMore dec' txt
        | otherwise -> pullMore dec' txt
      Right (Done _) -> throwError $ HQRunnerError UnexpectedEndOfInput
    emit event cursor = do
      S.yield event
      if event == closing
        then pure cursor
        else case event of
          JSONBeginArray -> nested JSONEndArray cursor
          JSONBeginObject -> nested JSONEndObject cursor
          _ -> go cursor
    nested end cursor = takeContainerFrom end cursor >>= go
    pullMore dec txt = do
      result <- lift (S.next txt)
      case result of
        Left () -> finishTake dec
        Right (chunk, rest) -> case Decoder.feed chunk dec of
          Left err -> throwError (HQDecodeError err)
          Right (Emit event dec') -> emit event $ Cursor [] dec' rest
          Right (NeedInput dec') -> stepMore dec' rest
          Right (Done _) -> throwError $ HQRunnerError UnexpectedEndOfInput
    -- Mirror 'drainAtEnd': a value completed exactly at end of input
    -- still yields its final event; anything else ends the take the
    -- same way the event-stream takes did.
    finishTake dec = case finish dec of
      Left UnexpectedEnd
        | decoderPhase dec == DecoderPhaseValue && null (decoderStack dec) ->
            throwError $ HQRunnerError UnexpectedEndOfInput
      Left err -> throwError (HQDecodeError err)
      Right (Done _) -> throwError $ HQRunnerError UnexpectedEndOfInput
      Right (NeedInput _) -> throwError (HQDecodeError UnexpectedEnd)
      Right (Emit event dec') -> emit event $ Cursor [] dec' (pure ())

-- | Format one event and yield its chunk, returning advanced contexts.
emitChunk ::
  EncoderConfig ->
  JSONEvent ->
  EncoderState ->
  ChunkStream (ExceptT HQError IO) EncoderState
emitChunk config event st = do
  let (chunk, st') = formatEvent config st event
  S.yield chunk
  pure st'

-- | Take one complete value, transcribing it straight to chunks.
takeValueChunks ::
  EncoderConfig ->
  Cursor ->
  EncoderState ->
  ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
takeValueChunks config cursor st = do
  (event, cursor') <- lift (pullOne cursor)
  st' <- emitChunk config event st
  case event of
    JSONBeginArray -> takeContainerChunks config JSONEndArray cursor' st'
    JSONBeginObject -> takeContainerChunks config JSONEndObject cursor' st'
    JSONEndArray -> throwError $ HQRunnerError UnexpectedEndOfArray
    JSONEndObject -> throwError $ HQRunnerError UnexpectedEndOfObject
    JSONObjectKey _ -> throwError $ HQRunnerError UnexpectedObjectKey
    _ -> pure (st', cursor')

-- | Batch threshold for transcribed chunks, in estimated chars: past
-- this, pending builders yield as one chunk instead of accumulating.
-- Amortizes stream steps over many events; output bytes are
-- unaffected (chunk boundaries never change them).
batchSize :: Int
batchSize = 8192

-- | Take a container body, transcribing text straight to chunks: the
-- decode and format steps fuse per event with no intermediate event
-- stream. Formatted builders batch into one chunk per 'batchSize'
-- chars instead of one chunk per event, amortizing stream steps over
-- many events; output bytes are unaffected (chunk boundaries never
-- change them).
takeContainerChunks ::
  EncoderConfig ->
  JSONEvent ->
  Cursor ->
  EncoderState ->
  ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
takeContainerChunks config closing c st0 = go c st0 mempty 0
  where
    go ::
      Cursor ->
      EncoderState ->
      Builder ->
      Int ->
      ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
    go (Cursor buf dec txt) st pend pendSize = case buf of
      event : rest -> emit event (Cursor rest dec txt) st pend pendSize
      [] -> stepMore dec txt st pend pendSize
    stepMore ::
      DecoderState ->
      StreamIO Text () ->
      EncoderState ->
      Builder ->
      Int ->
      ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
    stepMore dec txt st pend pendSize = case Decoder.step dec of
      Left err -> throwError (HQDecodeError err)
      Right (Emit event dec') ->
        let newCursor = Cursor [] dec' txt
         in emit event newCursor st pend pendSize
      Right (NeedInput dec')
        | not (T.null (decoderInput dec')) -> stepMore dec' txt st pend pendSize
        | otherwise -> pullMore dec' txt st pend pendSize
      Right (Done _) -> throwError $ HQRunnerError UnexpectedEndOfInput
    emit ::
      JSONEvent ->
      Cursor ->
      EncoderState ->
      Builder ->
      Int ->
      ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
    emit event cursor st pend pendSize = do
      let (Chunk b s, st') = formatEvent config st event
          pend' = pend <> b
          pendSize' = pendSize + s
      if event == closing
        then flush pend' pendSize' >> pure (st', cursor)
        else case event of
          JSONBeginArray -> nested JSONEndArray cursor st' pend' pendSize'
          JSONBeginObject -> nested JSONEndObject cursor st' pend' pendSize'
          _ | pendSize' >= batchSize -> do
            flush pend' pendSize'
            go cursor st' mempty 0
          _ -> go cursor st' pend' pendSize'
    nested ::
      JSONEvent ->
      Cursor ->
      EncoderState ->
      Builder ->
      Int ->
      ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
    nested end cursor st pend pendSize = do
      -- Flush before descending so nested chunks yield after us.
      flush pend pendSize
      (st', cursor') <- takeContainerChunks config end cursor st
      go cursor' st' mempty 0
    pullMore ::
      DecoderState ->
      StreamIO Text () ->
      EncoderState ->
      Builder ->
      Int ->
      ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
    pullMore dec txt st pend pendSize = do
      result <- lift (S.next txt)
      case result of
        Left () -> finishTake dec st pend pendSize
        Right (chunk, rest) -> case Decoder.feed chunk dec of
          Left err -> throwError (HQDecodeError err)
          Right (Emit event dec') ->
            let newCursor = Cursor [] dec' rest
             in emit event newCursor st pend pendSize
          Right (NeedInput dec') -> stepMore dec' rest st pend pendSize
          Right (Done _) -> throwError $ HQRunnerError UnexpectedEndOfInput
    -- Mirror 'drainAtEnd', emitting the final event as a chunk.
    finishTake ::
      DecoderState ->
      EncoderState ->
      Builder ->
      Int ->
      ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
    finishTake dec st pend pendSize = case finish dec of
      Left UnexpectedEnd
        | decoderPhase dec == DecoderPhaseValue && null (decoderStack dec) ->
            throwError $ HQRunnerError UnexpectedEndOfInput
      Left err -> throwError $ HQDecodeError err
      Right (Done _) -> throwError $ HQRunnerError UnexpectedEndOfInput
      Right (NeedInput _) -> throwError $ HQDecodeError UnexpectedEnd
      Right (Emit event dec') ->
        let newCursor = Cursor [] dec' (pure ())
         in emit event newCursor st pend pendSize
    flush :: Builder -> Int -> ChunkStream (ExceptT HQError IO) ()
    flush pend pendSize = when (pendSize > 0) $ S.yield $ Chunk pend pendSize
