{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner.Take where

import Control.Monad.Error.Class (MonadError (throwError))
import HQ.Error (HQError (..))
import HQ.JSON.Decoder (DecodeError (..), Decoder (..), DecoderResult (..), DecoderState (..), Next (..), StreamIO, finish, pullEvent)
import qualified HQ.JSON.Decoder as Decoder
import HQ.JSON.Encoder (ChunkStream, EncodeCtx, EncoderConfig, formatEvent)
import HQ.JSON.Event (JSONEvent (..))
import HQ.Runner.Cursor (Continuation, Cursor (..), EventStream)
import HQ.Runner.Error (RunnerError (..))
import Relude hiding (Compose, id, many, some, state)
import qualified Streaming.Prelude as S

-- | Pull a single event for bulk takes. Used once per taken value
-- (not per event), so the intermediate tuple is negligible.
pullOne :: [JSONEvent] -> Decoder -> StreamIO Text () -> ExceptT HQError IO (JSONEvent, [JSONEvent], Decoder, StreamIO Text ())
pullOne (event : buffered) decoder text = pure (event, buffered, decoder, text)
pullOne [] decoder text = do
  pulled <- pullEvent decoder text
  case pulled of
    EndOfInput -> throwError (HQRunnerError UnexpectedEndOfInput)
    NextEvent event decoder' rest -> pure (event, [], decoder', rest)

-- | Stream the events of exactly one complete JSON value, returning the
-- cursor positioned immediately after it. Lazy: a consumer that stops
-- early (e.g. @S.take 1@) pulls only what it needs.
takeValue :: Continuation
takeValue (Cursor buffered decoder text) = do
  (event, buffered', decoder', text') <- lift (pullOne buffered decoder text)
  S.yield event
  case event of
    JSONBeginArray -> takeContainerFrom JSONEndArray buffered' decoder' text'
    JSONBeginObject -> takeContainerFrom JSONEndObject buffered' decoder' text'
    JSONEndArray -> throwError (HQRunnerError UnexpectedEndOfArray)
    JSONEndObject -> throwError (HQRunnerError UnexpectedEndOfObject)
    JSONObjectKey _ -> throwError (HQRunnerError UnexpectedObjectKey)
    _ -> pure (Cursor buffered' decoder' text')

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
takeContainerFrom :: JSONEvent -> [JSONEvent] -> Decoder -> StreamIO Text () -> EventStream Cursor
takeContainerFrom closing = go
  where
    go :: [JSONEvent] -> Decoder -> StreamIO Text () -> EventStream Cursor
    go buf dec txt = case buf of
      event : rest -> emit event rest dec txt
      [] -> case Decoder.step dec of
        Left err -> throwError (HQDecodeError err)
        Right (Emit event dec') -> emit event [] dec' txt
        Right (NeedInput dec') -> pullMore dec' txt
        Right (Done _) -> throwError (HQRunnerError UnexpectedEndOfInput)
    emit event buf dec txt = do
      S.yield event
      if event == closing
        then pure (Cursor buf dec txt)
        else case event of
          JSONBeginArray -> nested JSONEndArray buf dec txt
          JSONBeginObject -> nested JSONEndObject buf dec txt
          _ -> go buf dec txt
    nested end buf dec txt = do
      Cursor buf' dec' txt' <- takeContainerFrom end buf dec txt
      takeContainerFrom closing buf' dec' txt'
    pullMore dec txt = do
      result <- lift (S.next txt)
      case result of
        Left () -> finishTake dec
        Right (chunk, rest) -> case Decoder.feed chunk dec of
          Left err -> throwError (HQDecodeError err)
          Right (Emit event dec') -> emit event [] dec' rest
          Right (NeedInput dec') -> pullMore dec' rest
          Right (Done _) -> throwError (HQRunnerError UnexpectedEndOfInput)
    -- Mirror 'drainAtEnd': a value completed exactly at end of input
    -- still yields its final event; anything else ends the take the
    -- same way the event-stream takes did.
    finishTake dec = case finish dec of
      Left UnexpectedEnd
        | decoderState dec == DecoderStateValue && null (decoderStack dec) ->
            throwError (HQRunnerError UnexpectedEndOfInput)
      Left err -> throwError (HQDecodeError err)
      Right (Done _) -> throwError (HQRunnerError UnexpectedEndOfInput)
      Right (NeedInput _) -> throwError (HQDecodeError UnexpectedEnd)
      Right (Emit event dec') -> emit event [] dec' (pure ())

-- | Format one event and yield its chunk, returning advanced contexts.
emitChunk :: EncoderConfig -> JSONEvent -> [EncodeCtx] -> ChunkStream (ExceptT HQError IO) [EncodeCtx]
emitChunk config event ctxs = do
  let (chunk, ctxs') = formatEvent config ctxs event
  S.yield chunk
  pure ctxs'

-- | Take one complete value, transcribing it straight to chunks.
takeValueChunks :: EncoderConfig -> Cursor -> [EncodeCtx] -> ChunkStream (ExceptT HQError IO) ([EncodeCtx], Cursor)
takeValueChunks config (Cursor buffered decoder text) ctxs = do
  (event, buffered', decoder', text') <- lift (pullOne buffered decoder text)
  ctxs' <- emitChunk config event ctxs
  case event of
    JSONBeginArray -> takeContainerChunks config JSONEndArray buffered' decoder' text' ctxs'
    JSONBeginObject -> takeContainerChunks config JSONEndObject buffered' decoder' text' ctxs'
    JSONEndArray -> throwError (HQRunnerError UnexpectedEndOfArray)
    JSONEndObject -> throwError (HQRunnerError UnexpectedEndOfObject)
    JSONObjectKey _ -> throwError (HQRunnerError UnexpectedObjectKey)
    _ -> pure (ctxs', Cursor buffered' decoder' text')

-- | Take a container body, transcribing text straight to chunks: the
-- decode and format steps fuse per event with no intermediate event
-- stream.
takeContainerChunks :: EncoderConfig -> JSONEvent -> [JSONEvent] -> Decoder -> StreamIO Text () -> [EncodeCtx] -> ChunkStream (ExceptT HQError IO) ([EncodeCtx], Cursor)
takeContainerChunks config closing = go
  where
    go buf dec txt ctx = case buf of
      event : rest -> emit event rest dec txt ctx
      [] -> case Decoder.step dec of
        Left err -> throwError (HQDecodeError err)
        Right (Emit event dec') -> emit event [] dec' txt ctx
        Right (NeedInput dec') -> pullMore dec' txt ctx
        Right (Done _) -> throwError (HQRunnerError UnexpectedEndOfInput)
    emit event buf dec txt ctx = do
      ctx' <- emitChunk config event ctx
      if event == closing
        then pure (ctx', Cursor buf dec txt)
        else case event of
          JSONBeginArray -> nested JSONEndArray buf dec txt ctx'
          JSONBeginObject -> nested JSONEndObject buf dec txt ctx'
          _ -> go buf dec txt ctx'
    nested end buf dec txt ctx = do
      (ctx', Cursor buf' dec' txt') <- takeContainerChunks config end buf dec txt ctx
      takeContainerChunks config closing buf' dec' txt' ctx'
    pullMore dec txt ctx = do
      result <- lift (S.next txt)
      case result of
        Left () -> finishTake dec ctx
        Right (chunk, rest) -> case Decoder.feed chunk dec of
          Left err -> throwError (HQDecodeError err)
          Right (Emit event dec') -> emit event [] dec' rest ctx
          Right (NeedInput dec') -> pullMore dec' rest ctx
          Right (Done _) -> throwError (HQRunnerError UnexpectedEndOfInput)
    -- Mirror 'drainAtEnd', emitting the final event as a chunk.
    finishTake dec ctx = case finish dec of
      Left UnexpectedEnd
        | decoderState dec == DecoderStateValue && null (decoderStack dec) ->
            throwError (HQRunnerError UnexpectedEndOfInput)
      Left err -> throwError (HQDecodeError err)
      Right (Done _) -> throwError (HQRunnerError UnexpectedEndOfInput)
      Right (NeedInput _) -> throwError (HQDecodeError UnexpectedEnd)
      Right (Emit event dec') -> emit event [] dec' (pure ()) ctx
