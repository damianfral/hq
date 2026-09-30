{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner.Cursor where

import Control.Monad.Error.Class (MonadError (throwError))
import HQ.Error (HQError (..))
import HQ.JSON.Decoder (DecoderState (..), Next (..), StreamIO, finishValue, pullEvent)
import HQ.JSON.Encoder (ChunkStream, EncoderState)
import HQ.JSON.Event (JSONEvent (..), matchingClose)
import HQ.JSON.Skip (skipContainerText, skipMemberValueText)
import HQ.Runner.Error (RunnerError (..))
import Relude hiding (Compose, id, many, some, state)
import Streaming (Of, Stream)

-- | The house stream type: 'Stream' over 'ExceptT HQError IO', capable
-- of failing with an 'HQError'. See 'StreamIO'.
type EventStream r = Stream (Of JSONEvent) (ExceptT HQError IO) r

-- | Input cursor: pushed-back events with the decoder and text
-- positioned after them. Navigation peeks at events through
-- 'pullCursor'; bulk take loops decode forward; skipped regions never
-- decode into events at all.
data Cursor = Cursor ![JSONEvent] !DecoderState (StreamIO Text ())

-- | Interpret an optic against the JSON value at the cursor,
-- yielding the taken events and returning the advanced cursor.
type Continuation = Cursor -> EventStream Cursor

-- | Rewrite output: chunk stream returning advanced encoder contexts
-- and cursor. Passthrough regions transcribe text straight to chunks
-- without an intermediate event stream.
type RewriteContinuation = Cursor -> EncoderState -> ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)

-- | Pull one event for navigation. Buffered events come first;
-- otherwise the decoder drives forward. Returns 'Nothing' at clean
-- end of input.
pullCursor :: Cursor -> ExceptT HQError IO (Maybe (JSONEvent, Cursor))
pullCursor (Cursor (event : buffered) decoder text) =
  pure (Just (event, Cursor buffered decoder text))
pullCursor (Cursor [] decoder text) = do
  pulled <- pullEvent decoder text
  case pulled of
    EndOfInput -> pure Nothing
    NextEvent event decoder' rest -> pure (Just (event, Cursor [] decoder' rest))

-- | Push an event back for the continuation to see.
pushCursor :: JSONEvent -> Cursor -> Cursor
pushCursor event (Cursor buffered decoder text) = Cursor (event : buffered) decoder text

-- | Pull one event inside an object body: 'Left' rest on 'JSONEndObject',
-- 'Right' key and cursor after it on 'JSONObjectKey'. Throws
-- 'UnexpectedEndReadingObject' on end of input and 'InvalidObject' on
-- anything else. Shared by every object walk in folding and rewriting.
expectObjectStep :: Cursor -> ExceptT HQError IO (Either Cursor (Text, Cursor))
expectObjectStep input = do
  result <- pullCursor input
  case result of
    Nothing -> throwError $ HQRunnerError UnexpectedEndReadingObject
    Just (JSONEndObject, rest) -> pure (Left rest)
    Just (JSONObjectKey key, rest) -> pure (Right (key, rest))
    Just _ -> throwError $ HQRunnerError InvalidObject

-- | Pull one event inside an array body: 'Left' rest on 'JSONEndArray',
-- 'Right' event and cursor after it otherwise. Throws
-- 'UnexpectedEndReadingArray' on end of input. Shared by every array
-- walk in folding and rewriting.
expectArrayStep :: Cursor -> ExceptT HQError IO (Either Cursor (JSONEvent, Cursor))
expectArrayStep input = do
  result <- pullCursor input
  case result of
    Nothing -> throwError $ HQRunnerError UnexpectedEndReadingArray
    Just (JSONEndArray, rest) -> pure (Left rest)
    Just (event, rest) -> pure (Right (event, rest))

-- | Consume one complete value without yielding its events.
skipValue :: Cursor -> EventStream Cursor
skipValue = lift . skipValueE

-- | 'skipValue' in 'ExceptT'. Text-level skipping needs the container
-- body still ahead in the text; replayed cursors drain event by event.
skipValueE :: Cursor -> ExceptT HQError IO Cursor
skipValueE input = do
  result <- pullCursor input
  case result of
    Nothing -> throwError $ HQRunnerError UnexpectedEndOfInput
    Just (event, Cursor buffered decoder text) ->
      skipEvent event buffered decoder text
  where
    skipEvent event buffered decoder text = case matchingClose event of
      Just closing
        | null buffered -> skipOpened event decoder text buffered
        | otherwise -> drainNested closing (Cursor buffered decoder text)
      Nothing -> case event of
        JSONEndArray -> throwError $ HQRunnerError UnexpectedEndOfArray
        JSONEndObject -> throwError $ HQRunnerError UnexpectedEndOfObject
        JSONObjectKey _ -> throwError $ HQRunnerError UnexpectedObjectKey
        _ -> pure (Cursor buffered decoder text)
    skipOpened event decoder text buffered = do
      (decoder', rest) <- skipContainerText event decoder text
      pure (Cursor buffered decoder' rest)
    -- \| Consume one value event by event, without touching the text.
    drainValue :: Cursor -> ExceptT HQError IO Cursor
    drainValue stream = do
      result <- pullCursor stream
      case result of
        Nothing -> throwError $ HQRunnerError UnexpectedEndOfInput
        Just (event, rest) -> case matchingClose event of
          Just closing -> drainNested closing rest
          Nothing -> case event of
            JSONEndArray ->
              throwError $ HQRunnerError UnexpectedEndOfArray
            JSONEndObject ->
              throwError $ HQRunnerError UnexpectedEndOfObject
            JSONObjectKey _ ->
              throwError $ HQRunnerError UnexpectedObjectKey
            _ -> pure rest
    -- \| Consume a container body event by event up to its closing event.
    drainNested :: JSONEvent -> Cursor -> ExceptT HQError IO Cursor
    drainNested closing stream = do
      result <- pullCursor stream
      case result of
        Nothing -> throwError $ HQRunnerError UnexpectedEndOfInput
        Just (event, rest)
          | event == closing -> pure rest
          | otherwise -> case event of
              JSONObjectKey _
                | closing == JSONEndObject -> do
                    after <- drainValue rest
                    drainNested closing after
                | otherwise -> throwError $ HQRunnerError UnexpectedObjectKey
              _ -> do
                after <- drainValue (pushCursor event rest)
                drainNested closing after

-- | Skip an object member value at the text level, colon included.
skipMemberValue :: Cursor -> EventStream Cursor
skipMemberValue = lift . skipMemberValueE

skipMemberValueE :: Cursor -> ExceptT HQError IO Cursor
skipMemberValueE (Cursor buffered decoder@DecoderState {..} text)
  | null buffered = do
      (remainder, rest) <-
        skipMemberValueText decoderNestDepth decoderStack decoderInput text
      pure $ Cursor [] (finishValue decoder {decoderInput = remainder}) rest
  | otherwise = skipValueE (Cursor buffered decoder text)

--------------------------------------------------------------------------------
-- Shared walk combinators (single implementation for Fold/Rewrite)
--------------------------------------------------------------------------------

-- | Pull one event; on end return the input unchanged, otherwise run the handler.
-- Unifies the 7x preamble in Fold (@runField/runEach/...@).
onEventOrEnd :: Cursor -> ((JSONEvent, Cursor) -> EventStream Cursor) -> EventStream Cursor
onEventOrEnd input f = do
  result <- lift (pullCursor input)
  case result of
    Nothing -> pure input
    Just pair -> f pair

-- | Walk an object body step by step; @body@ handles each member key,
-- returning the cursor to continue from. Emits nothing, returns the
-- cursor after @JSONEndObject@.
traverseObject :: Cursor -> ((Text, Cursor) -> EventStream Cursor) -> EventStream Cursor
traverseObject input body = go input
  where
    go stream = do
      step <- lift (expectObjectStep stream)
      case step of
        Left rest -> pure rest
        Right (key, rest) -> do
          after <- body (key, rest)
          go after

-- | Walk an array body step by step; @body@ handles each element event,
-- returning the cursor to continue from.
traverseArray :: Cursor -> ((JSONEvent, Cursor) -> EventStream Cursor) -> EventStream Cursor
traverseArray input body = go input
  where
    go stream = do
      step <- lift (expectArrayStep stream)
      case step of
        Left rest -> pure rest
        Right (event, rest) -> do
          after <- body (event, rest)
          go after

-- | Consume the rest of an object body (all remaining members).
skipRestOfObject :: Cursor -> EventStream Cursor
skipRestOfObject input = traverseObject input $ \(_, rest) -> skipMemberValue rest

-- | Consume the rest of an array body (all remaining elements).
skipRestOfArray :: Cursor -> EventStream Cursor
skipRestOfArray input = traverseArray input $ \(event, rest) -> skipValue (pushCursor event rest)
