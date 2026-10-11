{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner.Cursor where

import HQ.Early (Early, leave)
import HQ.Error (HQError (..))
import HQ.JSON.Decoder (DecoderState (..), Next (..), NextEmitted (..), StreamIO, finishValue, initialDecoder, pullEvent)
import HQ.JSON.Decoder.Skip (SkipRest (..), skipContainerText, skipMemberValueText)
import HQ.JSON.Encoder (BSStream, EmitState)
import HQ.JSON.Event (JSONEvent (..), matchingClose)
import HQ.Runner.Error (RunnerError (..))
import Relude hiding (Compose, id, many, some, state)
import Streaming (Of, Stream)

-- | The house stream type: 'Stream' over 'IO'. Failures abort via
-- 'HQ.Early' exceptions instead of threading 'Either'. See 'StreamIO'.
type EventStream r = Stream (Of JSONEvent) IO r

-- | Input cursor: pushed-back events with the decoder and text
-- positioned after them. Navigation peeks at events through
-- 'pullCursor'; bulk take loops decode forward.
--
-- A strict record: same layout and strictness as the old positional
-- constructor, so existing positional patterns keep working.
data Cursor = Cursor
  { cursorBuffered :: [JSONEvent],
    cursorDecoder :: DecoderState,
    cursorText :: StreamIO Text ()
  }

-- | Interpret an optic against the JSON value at the cursor,
-- yielding the taken events and returning the advanced cursor.
type Continuation = Cursor -> EventStream Cursor

-- | Rewrite output: byte stream returning advanced emit state
-- and cursor.
type RewriteContinuation = Cursor -> EmitState -> BSStream IO RewriteResult

-- | The rewrite pipeline's accumulator: emit state threaded through
-- byte emission alongside the cursor. A strict record (same layout as
-- the tuple it replaces), so the pair is unpacked and always in WHNF.
data RewriteResult = RewriteResult
  { rewriteEncoder :: EmitState,
    rewriteCursor :: Cursor
  }

-- | One navigation step: the pulled event with the cursor positioned
-- after it. A strict record (same layout as the tuple it replaces),
-- so the pair is unpacked and always in WHNF.
data Step = Step {stepEvent :: JSONEvent, stepCursor :: Cursor}

-- | One object member step: the member key with the cursor positioned
-- after it. A strict record (same layout as the tuple it replaces).
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
-- finished decoder over the remaining input, pulls one event and
-- pushes it back for the query to consume; 'Nothing' at clean end of
-- input (trailing whitespace included). Call between top-level values
-- only, where the event buffer is empty and the decoder stack is
-- balanced; anything else passes through for the query to handle
-- exactly as a single-shot run would.
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
expectArrayStep :: Early HQError -> Cursor -> IO (Either Cursor Step)
expectArrayStep early input = do
  result <- pullCursor early input
  case result of
    Nothing -> leave early $ HQRunnerError UnexpectedEndReadingArray
    Just (Step JSONEndArray rest) -> pure (Left rest)
    Just step -> pure (Right step)

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
    -- \| Consume one value event by event, without touching the text.
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
    -- \| Consume a container body event by event up to its closing event.
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
skipMemberValueE early (Cursor buffered decoder text)
  | null buffered = do
      (SkipRest remainder rest) <-
        skipMemberValueText early (decoderNestDepth decoder) (decoderStack decoder) (decoderInput decoder) text
      pure $ Cursor [] (finishValue decoder {decoderInput = remainder}) rest
  | otherwise = skipValueE early (Cursor buffered decoder text)

--------------------------------------------------------------------------------
-- Shared walk combinators (single implementation for Fold/Rewrite)
--------------------------------------------------------------------------------

-- | Pull one event; on end return the input unchanged, otherwise run the handler.
-- Unifies the 7x preamble in Fold (@runField/runEach/...@).
onEventOrEnd ::
  Early HQError ->
  Cursor ->
  (Step -> EventStream Cursor) ->
  EventStream Cursor
onEventOrEnd early input f = do
  result <- lift (pullCursor early input)
  case result of
    Nothing -> pure input
    Just step -> f step

-- | Match the next event against a table of opens; anything else is
-- skipped as a whole value. Unifies the open-or-skip preamble in
-- Fold (@runField@/@runEach@/@runKeys@/@runValues@/@runIndex@).
-- Inlined so the table specializes per call site.
{-# INLINE onOpenOrSkip #-}
onOpenOrSkip ::
  Early HQError ->
  Cursor ->
  [(JSONEvent, Cursor -> EventStream Cursor)] ->
  EventStream Cursor
onOpenOrSkip early input arms =
  onEventOrEnd early input $ \(Step event rest) ->
    case [handle | (open, handle) <- arms, open == event] of
      (handle : _) -> handle rest
      [] -> skipValue early (pushCursor event rest)

-- | Loop pulling member steps until the container ends; @body@ handles
-- each step, returning the cursor to continue from. Shared by
-- 'traverseObject' and 'traverseArray'. Inlined so @next@/@body@
-- specialize per walk.
{-# INLINE walkMembers #-}
walkMembers ::
  (Cursor -> IO (Either Cursor s)) ->
  (s -> EventStream Cursor) ->
  Cursor ->
  EventStream Cursor
walkMembers next body = go
  where
    go stream = do
      step <- lift (next stream)
      case step of
        Left rest -> pure rest
        Right pair -> body pair >>= go

-- | Walk an object body step by step; @body@ handles each member key,
-- returning the cursor to continue from. Emits nothing, returns the
-- cursor after @JSONEndObject@.
traverseObject :: Early HQError -> Cursor -> (ObjectStep -> EventStream Cursor) -> EventStream Cursor
traverseObject early input body = walkMembers (expectObjectStep early) body input

-- | Walk an array body step by step; @body@ handles each element event,
-- returning the cursor to continue from.
traverseArray :: Early HQError -> Cursor -> (Step -> EventStream Cursor) -> EventStream Cursor
traverseArray early input body = walkMembers (expectArrayStep early) body input

-- | Consume the rest of an object body (all remaining members).
skipRestOfObject :: Early HQError -> Cursor -> EventStream Cursor
skipRestOfObject early input = traverseObject early input $ \(ObjectStep _ rest) -> skipMemberValue early rest

-- | Consume the rest of an array body (all remaining elements).
skipRestOfArray :: Early HQError -> Cursor -> EventStream Cursor
skipRestOfArray early input = traverseArray early input $ \(Step event rest) -> skipValue early (pushCursor event rest)
