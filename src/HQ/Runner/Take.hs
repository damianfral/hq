{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner.Take where

import Data.Text qualified as T
import HQ.Early (Early, leave)
import HQ.JSON.Decoder
import HQ.JSON.Decoder qualified as Decoder
import HQ.JSON.Event (JSONEvent (..), matchingClose)
import HQ.Runner.Cursor
import HQ.Runner.Error (RunnerError (..))
import Relude hiding (Compose, id, many, some, state)
import Streaming (Of, Stream)
import Streaming.Prelude qualified as S

-- | Pull a single event, with the advanced cursor.
pullOne :: Early HQError -> Cursor -> IO Step
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
  TerminalEmit (TerminalEmitted event dec') -> pure (event, dec')

-- | Stream the events of exactly one complete JSON value. Lazy: stops
-- pulling once the value is complete.
takeValue :: Early HQError -> Continuation
takeValue early cursor = do
  Step event cursor' <- lift (pullOne early cursor)
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
-- Thin wrapper over 'takeContainerGeneric' with 'S.yield' as the output
-- action and no extra state.
takeContainerFrom :: Early HQError -> JSONEvent -> Cursor -> EventStream Cursor
takeContainerFrom early closing cursor = do
  (cur, _) <- takeContainerGeneric early closing cursor () outputOne
  pure cur
  where
    outputOne :: JSONEvent -> () -> EventStream ()
    outputOne event () = S.yield event
{-# INLINE takeContainerFrom #-}

-- | Single event-by-event container loop shared by the read side
-- ('takeContainerFrom', output = 'S.yield') and the rewrite side
-- ('takeContainerChunks', output = 'emitChunk'): same decoder driving,
-- same errors, same termination; only the per-event output action and
-- the threaded state differ. The two wrappers above/below specialize
-- it, so both call sites keep their old performance: 'INLINE' dissolves
-- the loop into each caller, exposing the concrete output action.
{-# INLINE takeContainerGeneric #-}
takeContainerGeneric ::
  forall e s.
  Early HQError ->
  JSONEvent ->
  Cursor ->
  s ->
  (JSONEvent -> s -> Stream (Of e) IO s) ->
  Stream (Of e) IO (Cursor, s)
takeContainerGeneric early closing cursor0 st0 outputOne = go cursor0 st0
  where
    go :: Cursor -> s -> Stream (Of e) IO (Cursor, s)
    go (Cursor buf dec txt) st = case buf of
      event : rest -> emit event (Cursor rest dec txt) st
      [] -> stepMore dec txt st
    -- Step while the decoder holds pending input: mid-value pauses
    -- (e.g. a high surrogate awaiting its low half) must be stepped,
    -- not finished. Only a truly drained decoder may pull more text.
    -- Mirrors 'pullMore' in "HQ.Runner.Cursor".
    stepMore :: DecoderState -> StreamIO Text () -> s -> Stream (Of e) IO (Cursor, s)
    stepMore dec txt st = case Decoder.step dec of
      Left err -> lift (leave early $ HQDecodeError err)
      Right (Emit event dec') -> emit event (Cursor [] dec' txt) st
      Right (NeedInput dec')
        | hasPending dec' -> stepMore dec' txt st
        | otherwise -> pullMore dec' txt st
      Right (Done _) -> lift (leave early $ HQRunnerError UnexpectedEndOfInput)
    pullMore :: DecoderState -> StreamIO Text () -> s -> Stream (Of e) IO (Cursor, s)
    pullMore dec txt st = do
      result <- lift (S.next txt)
      case result of
        Left () -> finishTake dec st
        Right (chunk, rest) -> case Decoder.feed chunk dec of
          Left err -> lift (leave early (HQDecodeError err))
          Right (Emit event dec') -> emit event (Cursor [] dec' rest) st
          Right (NeedInput dec') -> stepMore dec' rest st
          Right (Done _) -> lift (leave early $ HQRunnerError UnexpectedEndOfInput)
    -- Mirror terminal 'finish' policy: a value completed exactly at end
    -- of input still yields its final event; anything else ends the
    -- take the same way the event-stream takes did.
    finishTake :: DecoderState -> s -> Stream (Of e) IO (Cursor, s)
    finishTake dec st = do
      (event, dec') <- lift (finishTakeEvent early dec)
      emit event (Cursor [] dec' (pure ())) st
    emit :: JSONEvent -> Cursor -> s -> Stream (Of e) IO (Cursor, s)
    emit !event cur st = do
      st' <- outputOne event st
      if event == closing
        then pure (cur, st')
        else case matchingClose event of
          Just end -> nested end cur st'
          Nothing -> go cur st'
    nested :: JSONEvent -> Cursor -> s -> Stream (Of e) IO (Cursor, s)
    nested end cur st = do
      (cur', st') <- takeContainerGeneric early end cur st outputOne
      go cur' st'
