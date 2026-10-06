{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Query where

import HQ.Optic
import HQ.Optic.AST
import HQ.Optic.OpticType (OpticType (..), canUseAs)
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

-- | The optic type a query requires.
queryOpticType :: Query -> OpticType
queryOpticType (Fold _) = OpticTraversal
queryOpticType (Preview _) = OpticPrism
queryOpticType (Over _ _) = OpticTraversal
queryOpticType (Delete _) = OpticTraversal

data TypeError
  = InvalidOpticType OpticType OpticType
  | InvalidTransformationType TransformationTypeError
  deriving (Eq, Show)

renderTypeError :: TypeError -> Text
renderTypeError (InvalidOpticType expected actual) =
  "optic mismatch: this query needs "
    <> opticTypeName expected
    <> " but the optic is "
    <> opticTypeName actual
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

opticTypeName :: OpticType -> Text
opticTypeName OpticLens = "a lens (exactly one target)"
opticTypeName OpticPrism = "a prism (at most one target)"
opticTypeName OpticAffineTraversal = "an affine traversal (at most one target)"
opticTypeName OpticTraversal = "a traversal"

valueTypeName :: ValueType -> Text
valueTypeName ValueObject = "object"
valueTypeName ValueArray = "array"
valueTypeName ValueString = "string"
valueTypeName ValueNumber = "number"
valueTypeName ValueBool = "boolean"
valueTypeName ValueNull = "null"
valueTypeName ValueAny = "any value"

-- | Check a query before running it: optic cardinality, @filter@
-- booleans, 'Over' composition.
typecheckQuery :: Query -> Either TypeError Query
typecheckQuery q =
  if canUseAs expected current
    then checkOptic q >>= checkTransformation
    else Left err
  where
    err = InvalidOpticType expected current
    expected = queryOpticType q
    current = inferOpticType (getOptic q)

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
