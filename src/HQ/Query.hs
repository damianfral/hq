{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Query where

import HQ.Optic
import HQ.Transformation (Transformation)
import HQ.Transformation.AST
import HQ.Transformation.TransformationType (ValueType (..))
import Relude hiding (Compose, many, not, or, some, subtract)

--------------------------------------------------------------------------------

data Query
  = Fold Optic
  | Preview Optic
  | -- | Apply a transformation to each of the focused values.
    Over Optic Transformation
  | Delete Optic
  deriving (Show, Eq)

getOptic :: Query -> Optic
getOptic (Fold optic) = optic
getOptic (Preview optic) = optic
getOptic (Over optic _) = optic
getOptic (Delete optic) = optic

newtype TypeError
  = InvalidTransformationType TransformationTypeError
  deriving (Eq, Show)

renderTypeError :: TypeError -> Text
renderTypeError (InvalidTransformationType (InvalidCompose t out inn)) =
  "transformation mismatch in "
    <> show t
    <> ": produces "
    <> valueTypeName out
    <> " but the next step expects "
    <> valueTypeName inn
renderTypeError (InvalidTransformationType (InvalidOr t out)) =
  "transformation mismatch in "
    <> show t
    <> ": both sides of or must produce booleans, but a branch produces "
    <> valueTypeName out
renderTypeError (InvalidTransformationType (InvalidAnd t out)) =
  "transformation mismatch in "
    <> show t
    <> ": both sides of and must produce booleans, but a branch produces "
    <> valueTypeName out
renderTypeError (InvalidTransformationType (InvalidXor t out)) =
  "transformation mismatch in "
    <> show t
    <> ": both sides of xor must produce booleans, but a branch produces "
    <> valueTypeName out
renderTypeError (InvalidTransformationType (InvalidFilter t out)) =
  "filter transformation "
    <> show t
    <> " must produce a boolean, but produces "
    <> valueTypeName out

valueTypeName :: ValueType -> Text
valueTypeName ValueObject = "object"
valueTypeName ValueArray = "array"
valueTypeName ValueString = "string"
valueTypeName ValueNumber = "number"
valueTypeName ValueBool = "boolean"
valueTypeName ValueNull = "null"
valueTypeName ValueAny = "any value"

-- | Check a query before running it: @filter@ booleans and 'Over'
-- composition types. Every query accepts any optic.
typecheckQuery :: Query -> Either TypeError Query
typecheckQuery q = checkOptic q >>= checkTransformation

checkOptic :: Query -> Either TypeError Query
checkOptic q = go (getOptic q) >> pure q
  where
    go :: Optic -> Either TypeError ()
    go (Filter o t) = checkPredicate t >> go o
    go (Compose l r) = go l >> go r
    go _ = pure ()
    checkPredicate :: Transformation -> Either TypeError ()
    checkPredicate t = case inferTransformationType t of
      Left e -> Left (InvalidTransformationType e)
      Right tt ->
        if transformationOutput tt == ValueBool
          then pure ()
          else Left (InvalidTransformationType (InvalidFilter t (transformationOutput tt)))

checkTransformation :: Query -> Either TypeError Query
checkTransformation q@(Over _ transformation) =
  case inferTransformationType transformation of
    Left transformationTypeError -> Left $ InvalidTransformationType transformationTypeError
    Right _ -> pure q
checkTransformation q = pure q
