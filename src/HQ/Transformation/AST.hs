{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Transformation.AST
  ( inferTransformationType,
    TransformationType (..),
    TransformationTypeError (..),
  )
where

import Data.Aeson.Types (Value (..))
import HQ.Transformation
import HQ.Transformation.TransformationType
import Relude hiding (Compose, Const, many, not, or, some, subtract, toStrict)

-- | Mismatched 'Compose' steps, keeping the offender for reporting.
data TransformationTypeError
  = -- | Right output vs left input mismatch; offender kept for reporting.
    InvalidCompose Transformation ValueType ValueType
  | -- | An 'Or' branch does not produce a boolean; offender kept.
    InvalidOr Transformation ValueType
  | -- | An 'And' branch does not produce a boolean; offender kept.
    InvalidAnd Transformation ValueType
  | -- | A 'Xor' branch does not produce a boolean; offender kept.
    InvalidXor Transformation ValueType
  | -- | A @filter@ predicate does not produce a boolean; offender kept.
    InvalidFilter Transformation ValueType
  deriving (Eq, Show)

-- | Infer a transformation's input/output types; 'Compose' steps must
-- line up (constants accept anything). Only the root type is ever
-- consumed, so no annotated tree is materialized.
inferTransformationType ::
  Transformation -> Either TransformationTypeError TransformationType
inferTransformationType = go
  where
    go (Add _) = pure $ TransformationType ValueNumber ValueNumber
    go (Multiply _) = pure $ TransformationType ValueNumber ValueNumber
    go (Subtract _) = pure $ TransformationType ValueNumber ValueNumber
    go (Divide _) = pure $ TransformationType ValueNumber ValueNumber
    go (ConcatString _) = pure $ TransformationType ValueString ValueString
    go (ConcatArray _) = pure $ TransformationType ValueArray ValueArray
    go Trim = pure $ TransformationType ValueString ValueString
    go (Replace _ _) = pure $ TransformationType ValueString ValueString
    go (StripPrefix _) = pure $ TransformationType ValueString ValueString
    go (StripSuffix _) = pure $ TransformationType ValueString ValueString
    go (IsPrefixOf _) = pure $ TransformationType ValueString ValueBool
    go (IsSuffixOf _) = pure $ TransformationType ValueString ValueBool
    go (IsInfixOf _) = pure $ TransformationType ValueString ValueBool
    go IsEmpty = pure $ TransformationType ValueArray ValueBool
    go ArrayLength = pure $ TransformationType ValueArray ValueNumber
    go ArrayReverse = pure $ TransformationType ValueArray ValueArray
    go ArrayUnique = pure $ TransformationType ValueArray ValueArray
    go (Equal _) = pure $ TransformationType ValueAny ValueBool
    go (Const v) = pure $ TransformationType ValueAny (valueType v)
    go Not = pure $ TransformationType ValueBool ValueBool
    go (Or left right) = boolPair Or InvalidOr left right
    go (And left right) = boolPair And InvalidAnd left right
    go (Xor left right) = boolPair Xor InvalidXor left right
    go (Compose left right) = do
      lt <- go left
      rt <- go right
      let conditions =
            [ transformationInput lt == ValueAny,
              transformationOutput rt == transformationInput lt
            ]
          inputType = transformationInput rt
          outputType = transformationOutput lt
      if getAny $ foldMap Any conditions
        then pure $ TransformationType inputType outputType
        else Left $ InvalidCompose (Compose left right) (transformationOutput rt) (transformationInput lt)

-- | Check a boolean connective: both branches must produce booleans;
-- the input type comes from the left branch.
boolPair ::
  (Transformation -> Transformation -> Transformation) ->
  (Transformation -> ValueType -> TransformationTypeError) ->
  Transformation ->
  Transformation ->
  Either TransformationTypeError TransformationType
boolPair ctor toErr left right = do
  lt <- inferTransformationType left
  rt <- inferTransformationType right
  case (transformationOutput lt, transformationOutput rt) of
    (ValueBool, ValueBool) ->
      pure $ TransformationType (transformationInput lt) ValueBool
    (ValueBool, other) ->
      Left $ toErr (ctor left right) other
    (other, _) ->
      Left $ toErr (ctor left right) other

-- | The 'ValueType' of an embedded JSON literal.
valueType :: Value -> ValueType
valueType (Object _) = ValueObject
valueType (Array _) = ValueArray
valueType (String _) = ValueString
valueType (Number _) = ValueNumber
valueType (Bool _) = ValueBool
valueType Null = ValueNull
