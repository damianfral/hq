{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Transformation.Error where

import Relude hiding (Const, many, not, or, some, subtract, toStrict)

-- | A transformation failure: wrong value type, non-boolean @or@
-- branch, or violated runner contract (string keys, boolean gates).
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

renderTransformationError :: TransformationError -> Text
renderTransformationError err = case err of
  ExpectedNumber -> "expected a number"
  ExpectedString -> "expected a string"
  ExpectedArray -> "expected an array"
  ExpectedBoolean -> "expected a boolean"
  OrBranchNotBoolean -> "expected a boolean result from the left side of or"
  KeyNotString -> "key transformation must yield a string"
  FilterNotBoolean -> "filter transformation must produce a boolean"
