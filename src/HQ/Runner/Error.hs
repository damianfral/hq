{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner.Error where

import Relude hiding (Compose, Const)

data RunnerError
  = -- | The event or cursor stream ran dry where more was required.
    UnexpectedEndOfInput
  | UnexpectedEndOfArray
  | UnexpectedEndOfObject
  | UnexpectedObjectKey
  | -- | Structural failures while navigating or rewriting.
    InvalidObject
  | EmptyValue
  | ExpectedMemberValue
  | UnexpectedEndReadingObject
  | UnexpectedEndReadingArray
  | -- | Input is not valid UTF-8.
    InvalidUtf8
  | -- | Event-list materialization failures ('eventsToValue').
    TrailingEventsAfterValue
  | ExpectedJSONValue
  | ExpectedObjectKeyEvent
  deriving (Eq, Show)

renderRunnerError :: RunnerError -> Text
renderRunnerError err = case err of
  UnexpectedEndOfInput -> "unexpected end of JSON input"
  UnexpectedEndOfArray -> "unexpected end of array"
  UnexpectedEndOfObject -> "unexpected end of object"
  UnexpectedObjectKey -> "unexpected object key"
  InvalidObject -> "invalid JSON object"
  EmptyValue -> "unexpected empty value"
  ExpectedMemberValue -> "unexpected end of input after object key"
  UnexpectedEndReadingObject -> "unexpected end of input while reading object"
  UnexpectedEndReadingArray -> "unexpected end of input while reading array"
  InvalidUtf8 -> "invalid UTF-8 input"
  TrailingEventsAfterValue -> "trailing events after a JSON value"
  ExpectedJSONValue -> "expected a JSON value"
  ExpectedObjectKeyEvent -> "expected an object key"
