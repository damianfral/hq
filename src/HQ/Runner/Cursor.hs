{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner.Cursor where

import HQ.Early (Early, leave)
import HQ.JSON.Decoder
import HQ.JSON.Decoder.Skip
import HQ.JSON.Event (JSONEvent (..), matchingClose)
import HQ.Runner.Error (RunnerError (..))
import Relude hiding (Compose, id, many, some, state)
import Streaming (Of, Stream)

-- | The house stream type: 'Stream' over 'IO'. Failures abort via
-- 'HQ.Early' exceptions instead of threading 'Either'. See 'StreamIO'.
type EventStream r = Stream (Of JSONEvent) IO r

-- | Input cursor: pushed-back events with the decoder and text
-- positioned after them.
data Cursor = Cursor
  { cursorBuffered :: [JSONEvent],
    cursorDecoder :: DecoderState,
    cursorText :: StreamIO Text ()
  }

-- | Interpret an optic against the JSON value at the cursor,
-- yielding the taken events and returning the advanced cursor.
type Continuation = Cursor -> EventStream Cursor

-- | One navigation step: the pulled event with the cursor positioned
-- after it.
data Step = Step {stepEvent :: JSONEvent, stepCursor :: Cursor}

-- | One object member step: the member key with the cursor positioned
-- after it.
data ObjectStep = ObjectStep {objectStepKey :: Text, objectStepCursor :: Cursor}

-- | Pull one event for navigation. Buffered events come first;
-- otherwise the decoder drives forward. Returns 'Nothing' at clean
-- end of input.
pullCursor :: Early HQError -> Cursor -> IO (Maybe Step)
pullCursor _ (Cursor (event : buffered) decoder text) =
  pure (Just (Step event (Cursor buffered decoder text)))
pullCursor early (Cursor [] decoder text) = do
  pulled <- pullEvent early decoder text
  case pulled of
    EndOfInput -> pure Nothing
    NextEvent (NextEmitted event decoder' rest) ->
      pure (Just (Step event (Cursor [] decoder' rest)))

-- | Push an event back for the continuation to see.
pushCursor :: JSONEvent -> Cursor -> Cursor
pushCursor event (Cursor buffered decoder text) =
  Cursor (event : buffered) decoder text

-- | Poll for the next top-level document at the cursor. Resets a
-- finished decoder over the remaining input and pushes the first event
-- back for the query to consume; 'Nothing' at clean end of input.
-- Call between top-level values only (empty buffer, balanced stack).
nextDocument :: Early HQError -> Cursor -> IO (Maybe Cursor)
nextDocument early cur@(Cursor buffered decoder text)
  | not (null buffered) || not (null (decoderStack decoder)) = pure (Just cur)
  | otherwise = do
      let cur' =
            Cursor [] (initialDecoder {decoderInput = decoderInput decoder}) text
      pulled <- pullCursor early cur'
      case pulled of
        Nothing -> pure Nothing
        Just (Step event rest) -> pure (Just (pushCursor event rest))

-- | Pull one event inside an object body: 'Left' rest on 'JSONEndObject',
-- 'Right' key and cursor after it on 'JSONObjectKey'. Throws
-- 'UnexpectedEndReadingObject' on end of input and 'InvalidObject' on
-- anything else. Shared by every object walk in folding and rewriting.
-- Inlined: runs once per member on every hot walk.
{-# INLINE expectObjectStep #-}
expectObjectStep :: Early HQError -> Cursor -> IO (Either Cursor ObjectStep)
expectObjectStep early input = do
  result <- pullCursor early input
  case result of
    Nothing -> leave early $ HQRunnerError UnexpectedEndReadingObject
    Just (Step JSONEndObject rest) -> pure (Left rest)
    Just (Step (JSONObjectKey key) rest) -> pure (Right (ObjectStep key rest))
    Just _ -> leave early $ HQRunnerError InvalidObject

-- | Pull one event inside an array body: 'Left' rest on 'JSONEndArray',
-- 'Right' event and cursor after it otherwise. Throws
-- 'UnexpectedEndReadingArray' on end of input. Shared by every array
-- walk in folding and rewriting.
-- Inlined: runs once per element on every hot walk.
{-# INLINE expectArrayStep #-}
expectArrayStep :: Early HQError -> Cursor -> IO (Either Cursor Step)
expectArrayStep early input = do
  result <- pullCursor early input
  case result of
    Nothing -> leave early $ HQRunnerError UnexpectedEndReadingArray
    Just (Step JSONEndArray rest) -> pure (Left rest)
    Just step' -> pure (Right step')

-- | Consume one complete value without yielding its events.
skipValue :: Early HQError -> Cursor -> EventStream Cursor
skipValue early = lift . skipValueE early

-- | 'skipValue' in 'IO'. Text-level skipping when the container
-- body is still ahead in the text (no replayed events); otherwise drain
-- event by event. Skipped regions never decode into events on the fast
-- path.
skipValueE :: Early HQError -> Cursor -> IO Cursor
skipValueE early input = do
  result <- pullCursor early input
  case result of
    Nothing -> leave early $ HQRunnerError UnexpectedEndOfInput
    Just (Step event (Cursor buffered decoder text)) ->
      skipEvent event buffered decoder text
  where
    skipEvent event buffered decoder text = case matchingClose event of
      Just closing
        | null buffered -> skipOpened event decoder text buffered
        | otherwise -> drainNested closing (Cursor buffered decoder text)
      Nothing -> case event of
        JSONEndArray -> leave early $ HQRunnerError UnexpectedEndOfArray
        JSONEndObject -> leave early $ HQRunnerError UnexpectedEndOfObject
        JSONObjectKey _ -> leave early $ HQRunnerError UnexpectedObjectKey
        _ -> pure (Cursor buffered decoder text)
    skipOpened event decoder text buffered = do
      (decoder', rest) <- skipContainerText early event decoder text
      pure (Cursor buffered decoder' rest)
    -- Event-by-event drain, without touching the text.
    drainValue :: Cursor -> IO Cursor
    drainValue stream = do
      result <- pullCursor early stream
      case result of
        Nothing -> leave early $ HQRunnerError UnexpectedEndOfInput
        Just (Step event rest) -> case matchingClose event of
          Just closing -> drainNested closing rest
          Nothing -> case event of
            JSONEndArray -> leave early $ HQRunnerError UnexpectedEndOfArray
            JSONEndObject -> leave early $ HQRunnerError UnexpectedEndOfObject
            JSONObjectKey _ -> leave early $ HQRunnerError UnexpectedObjectKey
            _ -> pure rest
    drainNested :: JSONEvent -> Cursor -> IO Cursor
    drainNested closing stream = do
      result <- pullCursor early stream
      case result of
        Nothing -> leave early $ HQRunnerError UnexpectedEndOfInput
        Just (Step event rest)
          | event == closing -> pure rest
          | otherwise -> case event of
              JSONObjectKey _
                | closing == JSONEndObject -> do
                    after <- drainValue rest
                    drainNested closing after
                | otherwise -> leave early $ HQRunnerError UnexpectedObjectKey
              _ -> do
                after <- drainValue (pushCursor event rest)
                drainNested closing after

-- | Skip an object member value at the text level, colon included.
skipMemberValue :: Early HQError -> Cursor -> EventStream Cursor
skipMemberValue early = lift . skipMemberValueE early

skipMemberValueE :: Early HQError -> Cursor -> IO Cursor
skipMemberValueE early (Cursor buffered decoder@DecoderState {..} text)
  | null buffered = do
      (SkipRest remainder rest) <-
        skipMemberValueText early decoderNestDepth decoderStack decoderInput text
      pure $ Cursor [] (finishValue decoder {decoderInput = remainder}) rest
  | otherwise = skipValueE early (Cursor buffered decoder text)
