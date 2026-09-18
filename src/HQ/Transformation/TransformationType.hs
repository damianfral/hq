{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Transformation.TransformationType where

import Relude hiding (Compose, id)

data ValueType
  = ValueObject
  | ValueArray
  | ValueString
  | ValueNumber
  | ValueBool
  | ValueNull
  | -- | Any value: the input type of a constant transformation, which
    -- accepts whatever value it is given.
    ValueAny
  deriving (Eq, Show)

-- | TransformationType of a transformation: its input and output value types.
data TransformationType = TransformationType
  { transformationInput :: ValueType,
    transformationOutput :: ValueType
  }
  deriving (Eq, Show)
