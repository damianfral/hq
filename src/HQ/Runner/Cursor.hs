{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner.Cursor where

import Control.Monad.Error.Class (MonadError (throwError))
import HQ.JSON.Decoder (Decoder (..), Pulled (..), StreamIO, finishValue, pullEvent)
import HQ.JSON.Encoder (ChunkStream, EncodeCtx)
import HQ.JSON.Event (JSONEvent (..))
import HQ.JSON.Skip (skipContainerText, skipMemberValueText)
import Relude hiding (Compose, id, many, some, state)

type ValueStreamF = StreamIO JSONEvent

type ValueStream = ValueStreamF ()

-- | Input cursor: pushed-back events with the decoder and text
-- positioned after them. Navigation peeks at events through
-- 'pullCursor'; bulk take loops decode forward; skipped regions never
-- decode into events at all.
data Cursor = Cursor ![JSONEvent] !Decoder (StreamIO Text ())

-- | Interpret an optic against the JSON value at the cursor,
-- yielding the taken events and returning the advanced cursor.
type K = Cursor -> ValueStreamF Cursor

-- | Rewrite output: chunk stream returning advanced encoder contexts
-- and cursor. Passthrough regions transcribe text straight to chunks
-- without an intermediate event stream.
type KRewrite = Cursor -> [EncodeCtx] -> ChunkStream (ExceptT Text IO) ([EncodeCtx], Cursor)

-- | Pull one event for navigation. Buffered events come first;
-- otherwise the decoder drives forward. Returns 'Nothing' at clean
-- end of input.
pullCursor :: Cursor -> ExceptT Text IO (Maybe (JSONEvent, Cursor))
pullCursor (Cursor (event : buffered) decoder text) =
  pure (Just (event, Cursor buffered decoder text))
pullCursor (Cursor [] decoder text) = do
  pulled <- pullEvent decoder text
  case pulled of
    PulledEnd -> pure Nothing
    PulledEvent event decoder' rest -> pure (Just (event, Cursor [] decoder' rest))

-- | Push an event back for the continuation to see.
pushCursor :: JSONEvent -> Cursor -> Cursor
pushCursor event (Cursor buffered decoder text) = Cursor (event : buffered) decoder text

-- | Consume one complete value without yielding its events.
--
-- The value's first event is pulled to dispatch on, then containers
-- are skipped at the text level ('skipContainerText') without
-- decoding their contents.
skipValue :: Cursor -> ValueStreamF Cursor
skipValue = lift . skipValueE

-- | 'skipValue' in 'ExceptT': shared by the event-stream and
-- chunk-stream pipelines.
--
-- Text-level skipping is only valid when the container body is still
-- ahead in the text, i.e. the buffer holds at most the peeked opening
-- event. Replayed cursors buffer whole values whose text is already
-- consumed; those are drained event by event instead.
skipValueE :: Cursor -> ExceptT Text IO Cursor
skipValueE input = do
  result <- pullCursor input
  case result of
    Nothing -> throwError "unexpected end of JSON input"
    Just (event, Cursor buffered decoder text) -> skipEvent event buffered decoder text
  where
    skipEvent event buffered decoder text = case event of
      JSONBeginArray
        | null buffered -> skipOpened event decoder text buffered
        | otherwise -> drainNested JSONEndArray (Cursor buffered decoder text)
      JSONBeginObject
        | null buffered -> skipOpened event decoder text buffered
        | otherwise -> drainNested JSONEndObject (Cursor buffered decoder text)
      JSONEndArray -> throwError "unexpected end of array"
      JSONEndObject -> throwError "unexpected end of object"
      JSONObjectKey _ -> throwError "unexpected object key"
      _ -> pure (Cursor buffered decoder text)
    skipOpened event decoder text buffered = do
      (decoder', rest) <- skipContainerText event decoder text
      pure (Cursor buffered decoder' rest)
    -- \| Consume one value event by event, without touching the text.
    drainValue :: Cursor -> ExceptT Text IO Cursor
    drainValue stream = do
      result <- pullCursor stream
      case result of
        Nothing -> throwError "unexpected end of JSON input"
        Just (JSONBeginArray, rest) -> drainNested JSONEndArray rest
        Just (JSONBeginObject, rest) -> drainNested JSONEndObject rest
        Just (JSONEndArray, _) -> throwError "unexpected end of array"
        Just (JSONEndObject, _) -> throwError "unexpected end of object"
        Just (JSONObjectKey _, _) -> throwError "unexpected object key"
        Just (_, rest) -> pure rest
    -- \| Consume a container body event by event up to its closing event.
    drainNested :: JSONEvent -> Cursor -> ExceptT Text IO Cursor
    drainNested closing stream = do
      result <- pullCursor stream
      case result of
        Nothing -> throwError "unexpected end of JSON input"
        Just (event, rest)
          | event == closing -> pure rest
          | otherwise -> case event of
              JSONObjectKey _
                | closing == JSONEndObject -> do
                    after <- drainValue rest
                    drainNested closing after
                | otherwise -> throwError "unexpected object key"
              _ -> do
                after <- drainValue (pushCursor event rest)
                drainNested closing after

-- | Skip an object member value starting right after its key: the
-- colon and value are consumed at the text level.
skipMemberValue :: Cursor -> ValueStreamF Cursor
skipMemberValue = lift . skipMemberValueE

-- | 'skipMemberValue' in 'ExceptT': shared by both pipelines.
skipMemberValueE :: Cursor -> ExceptT Text IO Cursor
skipMemberValueE (Cursor buffered decoder text)
  | null buffered = do
      (remainder, rest) <- skipMemberValueText (decoderStack decoder) (decoderInput decoder) text
      pure (Cursor [] (finishValue decoder {decoderInput = remainder}) rest)
  | otherwise = skipValueE (Cursor buffered decoder text)
