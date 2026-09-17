{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.JSON.Cursor (Cursor (..), fromStream, next, skipValue, consumeValue) where

import Control.Monad.Error.Class (MonadError (throwError))
import HQ.JSON.Decoder (StreamIO)
import HQ.JSON.Event (JSONEvent (..))
import Relude
import qualified Streaming.Prelude as S

newtype Cursor = Cursor {runCursor :: ExceptT Text IO (Maybe (JSONEvent, Cursor))}

fromStream :: StreamIO JSONEvent () -> Cursor
fromStream stream = Cursor $ do
  result <- S.next stream
  pure $ case result of
    Left () -> Nothing
    Right (event, rest) -> Just (event, fromStream rest)

next :: Cursor -> ExceptT Text IO (Maybe (JSONEvent, Cursor))
next = runCursor

skipValue :: Cursor -> ExceptT Text IO (Maybe Cursor)
skipValue c = do
  next c >>= \case
    Nothing -> pure Nothing
    Just (event, rest) -> case event of
      JSONNull -> pure (Just rest)
      JSONBool _ -> pure (Just rest)
      JSONNumber _ -> pure (Just rest)
      JSONString _ -> pure (Just rest)
      JSONBeginArray -> Just <$> skipContainer JSONEndArray rest
      JSONBeginObject -> Just <$> skipContainer JSONEndObject rest
      JSONObjectKey _ -> throwError "unexpected object key"
      JSONEndArray -> throwError "unexpected end of array"
      JSONEndObject -> throwError "unexpected end of object"

skipContainer :: JSONEvent -> Cursor -> ExceptT Text IO Cursor
skipContainer closing c =
  next c >>= \case
    Nothing -> throwError "unexpected end of JSON input"
    Just (event, rest)
      | event == closing -> pure rest
      | event == JSONBeginArray ->
          skipContainer JSONEndArray rest >>= skipContainer closing
      | event == JSONBeginObject ->
          skipContainer JSONEndObject rest >>= skipContainer closing
      | otherwise -> skipContainer closing rest

-- | Read all events for the current value, returning the collected
-- events and the cursor positioned immediately after the value.
--
-- This is used by 'eachArray'/'eachObject' to eagerly consume a
-- value's events into a list, avoiding the cursor aliasing bug where
-- both 'emitValue' and 'skipValue' would read from the same cursor.
consumeValue :: Cursor -> ExceptT Text IO (Maybe ([JSONEvent], Cursor))
consumeValue c = do
  next c >>= \case
    Nothing -> pure Nothing
    Just (event, rest) -> case event of
      JSONNull -> pure $ Just ([event], rest)
      JSONBool _ -> pure $ Just ([event], rest)
      JSONNumber _ -> pure $ Just ([event], rest)
      JSONString _ -> pure $ Just ([event], rest)
      JSONBeginArray -> consumeContainer JSONEndArray [event] rest
      JSONBeginObject -> consumeContainer JSONEndObject [event] rest
      JSONObjectKey _ -> throwError "unexpected object key"
      JSONEndArray -> throwError "unexpected end of array"
      JSONEndObject -> throwError "unexpected end of object"

consumeContainer ::
  JSONEvent ->
  [JSONEvent] ->
  Cursor ->
  ExceptT Text IO (Maybe ([JSONEvent], Cursor))
consumeContainer closing acc c = do
  next c >>= \case
    Nothing -> throwError "unexpected end of JSON input"
    Just (event, rest)
      | event == closing -> pure $ Just (reverse (event : acc), rest)
      | event == JSONBeginArray ->
          consumeContainer JSONEndArray [event] rest >>= \case
            Nothing -> pure Nothing
            Just (nestedEvents, afterNested) ->
              consumeContainer closing (reverse nestedEvents <> acc) afterNested
      | event == JSONBeginObject ->
          consumeContainer JSONEndObject [event] rest >>= \case
            Nothing -> pure Nothing
            Just (nestedEvents, afterNested) ->
              consumeContainer closing (reverse nestedEvents <> acc) afterNested
      | otherwise -> consumeContainer closing (event : acc) rest
