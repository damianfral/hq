{-# LANGUAGE ImportQualifiedPost #-}
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
