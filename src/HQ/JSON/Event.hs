{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.JSON.Event where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Scientific (Scientific)
import Data.Vector qualified as Vector
import HQ.Runner.Error (RunnerError (..))
import Relude hiding (Compose, id, many, some, state)

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------

data JSONEvent
  = JSONBeginObject
  | JSONEndObject
  | JSONBeginArray
  | JSONEndArray
  | JSONObjectKey Text
  | JSONNull
  | JSONBool Bool
  | JSONNumber Scientific
  | JSONString Text
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Event classifiers
--------------------------------------------------------------------------------

-- | Matching close for a container open; 'Nothing' for non-opens.
-- Single table for the open/close dispatch in Take/Cursor.
matchingClose :: JSONEvent -> Maybe JSONEvent
matchingClose = \case
  JSONBeginArray -> Just JSONEndArray
  JSONBeginObject -> Just JSONEndObject
  _ -> Nothing

-- | Parse exactly one value from events; trailing events are rejected.
eventsToValue :: [JSONEvent] -> Either RunnerError Value
eventsToValue events =
  case parseValue events of
    Left err -> Left err
    Right (value, []) -> Right value
    Right _ -> Left TrailingEventsAfterValue
  where
    parseValue :: [JSONEvent] -> Either RunnerError (Value, [JSONEvent])
    parseValue (JSONNull : rest) = Right (Null, rest)
    parseValue (JSONBool b : rest) = Right (Bool b, rest)
    parseValue (JSONNumber n : rest) = Right (Number n, rest)
    parseValue (JSONString s : rest) = Right (String s, rest)
    parseValue (JSONBeginArray : rest) =
      first (Array . Vector.fromList) <$> parseElements rest
    parseValue (JSONBeginObject : rest) =
      first (Object . KeyMap.fromList) <$> parseMembers rest
    parseValue _ = Left ExpectedJSONValue

    parseElements :: [JSONEvent] -> Either RunnerError ([Value], [JSONEvent])
    parseElements = parseDelimited JSONEndArray parseValue

    parseMembers :: [JSONEvent] -> Either RunnerError ([(Key.Key, Value)], [JSONEvent])
    parseMembers = parseDelimited JSONEndObject $ \case
      (JSONObjectKey key : rest) -> first (\v -> (Key.fromText key, v)) <$> parseValue rest
      _ -> Left ExpectedObjectKeyEvent

-- | Parse a delimited list ending in @end@; @parseOne@ parses a
-- single element (failing on anything else). Shared by 'parseElements'
-- and 'parseMembers', which differ only in end token and element shape.
-- Inlined so @parseOne@ specializes per list kind.
{-# INLINE parseDelimited #-}
parseDelimited ::
  JSONEvent ->
  ([JSONEvent] -> Either RunnerError (a, [JSONEvent])) ->
  [JSONEvent] ->
  Either RunnerError ([a], [JSONEvent])
parseDelimited end parseOne = go
  where
    go (ev : rest) | ev == end = Right ([], rest)
    go input = do
      (x, rest) <- parseOne input
      (xs, rest') <- go rest
      Right (x : xs, rest')

-- | Encode a JSON value as a sequence of events.
valueToEvents :: Value -> [JSONEvent]
valueToEvents Null = [JSONNull]
valueToEvents (Bool b) = [JSONBool b]
valueToEvents (Number n) = [JSONNumber n]
valueToEvents (String s) = [JSONString s]
valueToEvents (Array values) =
  JSONBeginArray : (concatMap valueToEvents (Vector.toList values) <> [JSONEndArray])
valueToEvents (Object members) =
  JSONBeginObject : (concatMap member (KeyMap.toList members) <> [JSONEndObject])
  where
    member (key, value) = JSONObjectKey (Key.toText key) : valueToEvents value
