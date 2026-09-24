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

-- | Splice output: chunk stream returning advanced encoder contexts
-- and cursor. Passthrough regions transcribe text straight to chunks
-- without an intermediate event stream.
type KSplice = Cursor -> [EncodeCtx] -> ChunkStream (ExceptT Text IO) ([EncodeCtx], Cursor)

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
skipValueE :: Cursor -> ExceptT Text IO Cursor
skipValueE input = do
  result <- pullCursor input
  case result of
    Nothing -> throwError "unexpected end of JSON input"
    Just (event, Cursor buffered decoder text) -> skipEvent event buffered decoder text
  where
    skipEvent event buffered decoder text = case event of
      JSONBeginArray -> skipOpened event decoder text buffered
      JSONBeginObject -> skipOpened event decoder text buffered
      JSONEndArray -> throwError "unexpected end of array"
      JSONEndObject -> throwError "unexpected end of object"
      JSONObjectKey _ -> throwError "unexpected object key"
      _ -> pure (Cursor buffered decoder text)
    skipOpened event decoder text buffered = do
      (decoder', rest) <- skipContainerText event decoder text
      pure (Cursor buffered decoder' rest)

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
