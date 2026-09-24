{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.JSON.Event where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Scientific (Scientific)
import qualified Data.Vector as Vector
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
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Event classifiers
--------------------------------------------------------------------------------

isString :: JSONEvent -> Bool
isString = \case
  JSONString _ -> True
  _ -> False

isNumber :: JSONEvent -> Bool
isNumber = \case
  JSONNumber _ -> True
  _ -> False

isBool :: JSONEvent -> Bool
isBool = \case
  JSONBool _ -> True
  _ -> False

isNull :: JSONEvent -> Bool
isNull = \case
  JSONNull -> True
  _ -> False

isArray :: JSONEvent -> Bool
isArray = \case
  JSONBeginArray -> True
  _ -> False

isObject :: JSONEvent -> Bool
isObject = \case
  JSONBeginObject -> True
  _ -> False

-- | Parse a JSON value from a sequence of events.
--
-- The whole event list must form exactly one value: any trailing events
-- are rejected.  The input is expected to come from the JSON parser, so
-- the failure cases only guard against malformed event sequences.
eventsToValue :: [JSONEvent] -> Either Text Value
eventsToValue events =
  case parseValue events of
    Left err -> Left err
    Right (value, []) -> Right value
    Right _ -> Left "trailing events after a JSON value"
  where
    parseValue :: [JSONEvent] -> Either Text (Value, [JSONEvent])
    parseValue (JSONNull : rest) = Right (Null, rest)
    parseValue (JSONBool b : rest) = Right (Bool b, rest)
    parseValue (JSONNumber n : rest) = Right (Number n, rest)
    parseValue (JSONString s : rest) = Right (String s, rest)
    parseValue (JSONBeginArray : rest) =
      first (Array . Vector.fromList) <$> parseElements rest
    parseValue (JSONBeginObject : rest) =
      first (Object . KeyMap.fromList) <$> parseMembers rest
    parseValue _ = Left "expected a JSON value"

    parseElements :: [JSONEvent] -> Either Text ([Value], [JSONEvent])
    parseElements (JSONEndArray : rest) = Right ([], rest)
    parseElements input = do
      (value, rest) <- parseValue input
      (values, rest') <- parseElements rest
      Right (value : values, rest')

    parseMembers :: [JSONEvent] -> Either Text ([(Key.Key, Value)], [JSONEvent])
    parseMembers (JSONEndObject : rest) = Right ([], rest)
    parseMembers (JSONObjectKey key : rest) = do
      (value, rest') <- parseValue rest
      (members, rest'') <- parseMembers rest'
      Right ((Key.fromText key, value) : members, rest'')
    parseMembers _ = Left "expected an object key"

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
