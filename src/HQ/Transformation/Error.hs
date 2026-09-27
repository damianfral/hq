{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | What can go wrong applying a transformation to a JSON value.
module HQ.Transformation.Error where

import Relude hiding (Const, many, not, or, some, subtract, toStrict)

-- | A transformation failure: the value has the wrong type for the
-- step, an @or@ branch is not boolean, or a runner-enforced contract
-- on the result is violated (a rewritten key must be a string, a
-- @filter@ gate must be boolean).
data TransformationError
  = ExpectedNumber
  | ExpectedString
  | ExpectedArray
  | ExpectedBoolean
  | -- | A non-boolean branch of @or@.
    OrBranchNotBoolean
  | -- | A key rewrite yielding a non-string.
    KeyNotString
  | -- | A @filter@ predicate yielding a non-boolean.
    FilterNotBoolean
  deriving (Eq, Show)

-- | Render a transformation failure with its historical message.
renderTransformationError :: TransformationError -> Text
renderTransformationError err = case err of
  ExpectedNumber -> "expected a number"
  ExpectedString -> "expected a string"
  ExpectedArray -> "expected an array"
  ExpectedBoolean -> "expected a boolean"
  OrBranchNotBoolean -> "expected a boolean result from the left side of or"
  KeyNotString -> "key transformation must yield a string"
  FilterNotBoolean -> "filter transformation must produce a boolean"
