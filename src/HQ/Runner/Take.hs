{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner.Take where

import Control.Monad.Error.Class (MonadError (throwError))
import Data.Char (isDigit)
import qualified Data.Text as T
import HQ.Error (HQError (..))
import HQ.JSON.Decoder (DecodeError (..), DecoderPhase (..), DecoderResult (..), DecoderState (..), Next (..), StreamIO, finish, finishValue, isWhitespace, pullEvent)
import qualified HQ.JSON.Decoder as Decoder
import HQ.JSON.Decoder.Skip (skipNumberCollect, skipStringCollect)
import HQ.JSON.Encoder (Builder, Chunk (..), ChunkStream, EncoderConfig, EncoderState, formatEvent, transcribeRawBytes, transcribeRawString)
import HQ.JSON.Event (JSONEvent (..), matchingClose)
import HQ.Runner.Cursor (Continuation, Cursor (..), EventStream, expectArrayStep, expectObjectStep, pullCursor)
import HQ.Runner.Error (RunnerError (..))
import Relude hiding (Compose, id, many, some, state)
import qualified Streaming.Prelude as S

-- | Pull a single event, with the advanced cursor.
pullOne :: Cursor -> ExceptT HQError IO (JSONEvent, Cursor)
pullOne (Cursor (event : buffered) decoder text) = pure (event, Cursor buffered decoder text)
pullOne (Cursor [] decoder text) = do
  pulled <- pullEvent decoder text
  case pulled of
    EndOfInput -> throwError $ HQRunnerError UnexpectedEndOfInput
    NextEvent event decoder' rest -> pure (event, Cursor [] decoder' rest)

-- | True when the decoder still holds pending input that must be
-- stepped before pulling more text. Single name for the 4x
-- @not (T.null (decoderInput ...))@ guard.
hasPending :: DecoderState -> Bool
hasPending dec = not (T.null (decoderInput dec))

-- | Shared 'finish' decision for take loops; throws on all non-emit
-- outcomes, returning the final event otherwise. Unifies the two
-- @finishTake@ copies plus @drainAtEnd@ semantics.
finishTakeEvent :: DecoderState -> ExceptT HQError IO (JSONEvent, DecoderState)
finishTakeEvent dec = case finish dec of
  Left UnexpectedEnd
    | decoderPhase dec == DecoderPhaseValue && null (decoderStack dec) ->
        throwError $ HQRunnerError UnexpectedEndOfInput
  Left err -> throwError (HQDecodeError err)
  Right (Done _) -> throwError $ HQRunnerError UnexpectedEndOfInput
  Right (NeedInput _) -> throwError (HQDecodeError UnexpectedEnd)
  Right (Emit event dec') -> pure (event, dec')

-- | Stream the events of exactly one complete JSON value. Lazy: stops
-- pulling once the value is complete.
takeValue :: Continuation
takeValue cursor = do
  (event, cursor') <- lift (pullOne cursor)
  S.yield event
  case matchingClose event of
    Just closing -> takeContainerFrom closing cursor'
    Nothing -> case event of
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

-- | Take a container body event by event, driving the decoder directly.
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
        | hasPending dec' -> stepMore dec' txt
        | otherwise -> pullMore dec' txt
      Right (Done _) -> throwError $ HQRunnerError UnexpectedEndOfInput
    emit event cursor = do
      S.yield event
      if event == closing
        then pure cursor
        else case matchingClose event of
          Just end -> nested end cursor
          Nothing -> go cursor
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
    finishTake dec = do
      (event, dec') <- lift (finishTakeEvent dec)
      emit event $ Cursor [] dec' (pure ())

emitChunk ::
  EncoderConfig ->
  JSONEvent ->
  EncoderState ->
  ChunkStream (ExceptT HQError IO) EncoderState
emitChunk config event st = do
  let (chunk, st') = formatEvent config st event
  S.yield chunk
  pure st'

--------------------------------------------------------------------------------
-- Shared chunk walk combinators (single implementation for Rewrite)
--------------------------------------------------------------------------------

-- | Pull one event; on end return unchanged, otherwise run the handler.
-- Chunk-stream analogue of 'HQ.Runner.Cursor.onEventOrEnd'.
onEventOrEndChunks ::
  Cursor ->
  EncoderState ->
  ((JSONEvent, Cursor) -> ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)) ->
  ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
onEventOrEndChunks input st f = do
  result <- lift (pullCursor input)
  case result of
    Nothing -> pure (st, input)
    Just pair -> f pair

-- | Walk an object body, emitting @JSONEndObject@ at the end.
-- Caller must have emitted @JSONBeginObject@ already.
traverseObjectChunks ::
  EncoderConfig ->
  Cursor ->
  EncoderState ->
  (Text -> Cursor -> EncoderState -> ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)) ->
  ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
traverseObjectChunks config input st body = go input st
  where
    go stream s = do
      step <- lift (expectObjectStep stream)
      case step of
        Left rest -> do
          s' <- emitChunk config JSONEndObject s
          pure (s', rest)
        Right (key, rest) -> do
          (s', after) <- body key rest s
          go after s'

-- | Walk an array body, emitting @JSONEndArray@ at the end.
traverseArrayChunks ::
  EncoderConfig ->
  Cursor ->
  EncoderState ->
  ((JSONEvent, Cursor) -> EncoderState -> ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)) ->
  ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
traverseArrayChunks config input st body = go input st
  where
    go stream s = do
      step <- lift (expectArrayStep stream)
      case step of
        Left rest -> do
          s' <- emitChunk config JSONEndArray s
          pure (s', rest)
        Right (event, rest) -> do
          (s', after) <- body (event, rest) s
          go after s'

-- | Emit a key then passthrough its value; the 4x pattern in
-- @rewriteMember@/pairs/allKeys@.
emitKeyAndTake ::
  EncoderConfig ->
  Text ->
  Cursor ->
  EncoderState ->
  ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
emitKeyAndTake config key rest s = do
  s' <- emitChunk config (JSONObjectKey key) s
  takeValueChunks config rest s'

-- | Take one complete value, transcribing it straight to chunks.
takeValueChunks ::
  EncoderConfig ->
  Cursor ->
  EncoderState ->
  ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
takeValueChunks config cursor st = case peekRawScalar cursor of
  Just (RawString afterQuote, dec, txt) ->
    takeRawStringChunks config afterQuote dec txt st
  Just (RawNumber atNumber, dec, txt) ->
    takeRawNumberChunks config atNumber dec txt st
  Nothing -> do
    (event, cursor') <- lift (pullOne cursor)
    st' <- emitChunk config event st
    case matchingClose event of
      Just closing -> takeContainerChunks config closing cursor' st'
      Nothing -> case event of
        JSONEndArray -> throwError $ HQRunnerError UnexpectedEndOfArray
        JSONEndObject -> throwError $ HQRunnerError UnexpectedEndOfObject
        JSONObjectKey _ -> throwError $ HQRunnerError UnexpectedObjectKey
        _ -> pure (st', cursor')

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
  EncoderConfig ->
  Text ->
  DecoderState ->
  StreamIO Text () ->
  EncoderState ->
  ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
takeRawStringChunks config afterQuote dec txt st = do
  (remainder, rawB, rawS, rest) <- lift (skipStringCollect afterQuote txt)
  let (chunk, st') = transcribeRawString config st rawB rawS
  S.yield chunk
  pure (st', Cursor [] (finishValue dec {decoderInput = remainder}) rest)

-- | Transcribe a peeked number value verbatim (no 'Scientific' roundtrip).
takeRawNumberChunks ::
  EncoderConfig ->
  Text ->
  DecoderState ->
  StreamIO Text () ->
  EncoderState ->
  ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
takeRawNumberChunks config atNumber dec txt st = do
  (remainder, rawB, rawS, rest) <- lift (skipNumberCollect atNumber txt)
  let (chunk, st') = transcribeRawBytes config st rawB rawS
  S.yield chunk
  pure (st', Cursor [] (finishValue dec {decoderInput = remainder}) rest)

-- | Pending-builder flush threshold; chunk boundaries never change bytes.
batchSize :: Int
batchSize = 8192

-- | Take a container body, fusing decode and format per event with no
-- intermediate event stream.
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
    go (Cursor !buf dec txt) !st !pend !pendSize = case buf of
      event : rest -> emit event (Cursor rest dec txt) st pend pendSize
      [] -> stepMore dec txt st pend pendSize
    stepMore ::
      DecoderState ->
      StreamIO Text () ->
      EncoderState ->
      Builder ->
      Int ->
      ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
    stepMore !dec !txt !st !pend !pendSize = case Decoder.step dec of
      Left err -> throwError (HQDecodeError err)
      Right (Emit event dec') ->
        let newCursor = Cursor [] dec' txt
         in emit event newCursor st pend pendSize
      Right (NeedInput dec')
        | hasPending dec' -> stepMore dec' txt st pend pendSize
        | otherwise -> pullMore dec' txt st pend pendSize
      Right (Done _) -> throwError $ HQRunnerError UnexpectedEndOfInput
    emit ::
      JSONEvent ->
      Cursor ->
      EncoderState ->
      Builder ->
      Int ->
      ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
    emit !event !cursor !st !pend !pendSize = do
      let (Chunk b s, st') = formatEvent config st event
          pend' = pend <> b
          pendSize' = pendSize + s
      if event == closing
        then flush pend' pendSize' >> pure (st', cursor)
        else case matchingClose event of
          Just end -> nested end cursor st' pend' pendSize'
          Nothing
            | pendSize' >= batchSize -> do
                flush pend' pendSize'
                go cursor st' mempty 0
            | otherwise -> go cursor st' pend' pendSize'
    nested ::
      JSONEvent ->
      Cursor ->
      EncoderState ->
      Builder ->
      Int ->
      ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
    nested !end !cursor !st !pend !pendSize = do
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
    pullMore !dec !txt !st !pend !pendSize = do
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
    finishTake !dec !st !pend !pendSize = do
      (event, dec') <- lift (finishTakeEvent dec)
      let newCursor = Cursor [] dec' (pure ())
       in emit event newCursor st pend pendSize
    flush :: Builder -> Int -> ChunkStream (ExceptT HQError IO) ()
    flush !pend !pendSize = when (pendSize > 0) $ S.yield $ Chunk pend pendSize
