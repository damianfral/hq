{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Query where

import Control.Comonad.Cofree (Cofree ((:<)))
import Data.Fix (Fix (..))
import HQ.Optic
import HQ.Optic.AST
import HQ.Optic.OpticType (OpticType (..), canUseAs)
import HQ.Transformation
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

-- | The optic type a query requires: how many focus points it expects.
-- 'typecheckQuery' compares this against the type derived from the
-- optic itself.
queryOpticType :: Query -> OpticType
queryOpticType (Fold _) = OpticTraversal
queryOpticType (Preview _) = OpticPrism
queryOpticType (Over _ _) = OpticTraversal
queryOpticType (Delete _) = OpticTraversal

data TypeError
  = InvalidOpticType OpticType OpticType
  | InvalidTransformationType TransformationTypeError
  deriving (Eq, Show)

-- | Render a 'TypeError' for the CLI.
renderTypeError :: TypeError -> Text
renderTypeError (InvalidOpticType expected actual) =
  "optic mismatch: this query needs "
    <> opticTypeName expected
    <> " but the optic is "
    <> opticTypeName actual
renderTypeError (InvalidTransformationType (InvalidCombine t out inn)) =
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
renderTypeError (InvalidTransformationType (InvalidFilter t out)) =
  "filter transformation "
    <> show t
    <> " must produce a boolean, but produces "
    <> valueTypeName out

-- | Human-readable cardinality names.
opticTypeName :: OpticType -> Text
opticTypeName OpticLens = "a lens (exactly one target)"
opticTypeName OpticPrism = "a prism (at most one target)"
opticTypeName OpticAffineTraversal = "an affine traversal (at most one target)"
opticTypeName OpticTraversal = "a traversal"

-- | Human-readable JSON value type names.
valueTypeName :: ValueType -> Text
valueTypeName ValueObject = "object"
valueTypeName ValueArray = "array"
valueTypeName ValueString = "string"
valueTypeName ValueNumber = "number"
valueTypeName ValueBool = "boolean"
valueTypeName ValueNull = "null"
valueTypeName ValueAny = "any value"

-- | Check a query before running it: the optic must be usable where the
-- query command expects it ('canUseAs'), every @filter@ predicate must
-- produce a boolean, and an 'Over' transformation must compose cleanly
-- ('buildTransformationAST').
typecheckQuery :: Query -> Either TypeError Query
typecheckQuery q =
  if canUseAs expected current
    then checkOptic q >>= checkTransformation
    else Left err
  where
    err = InvalidOpticType expected current
    expected = queryOpticType q
    OpticAST (current :< _) = buildOpticAST (getOptic q)

-- | Check every @filter@ predicate in the query's optic: each must
-- build cleanly and root-output a boolean.
checkOptic :: Query -> Either TypeError Query
checkOptic q = go (unOptic (getOptic q)) >> pure q
  where
    go :: Fix OpticF -> Either TypeError ()
    go (Fix f) = case f of
      Filter o t -> checkPredicate t >> go o
      Compose l r -> go l >> go r
      _ -> pure ()
    checkPredicate :: Transformation -> Either TypeError ()
    checkPredicate t = case buildTransformationAST t of
      Left e -> Left (InvalidTransformationType e)
      Right (TransformationAST (tt :< _)) ->
        if transformationOutput tt == ValueBool
          then pure ()
          else Left (InvalidTransformationType (InvalidFilter t (transformationOutput tt)))

-- | Check that the transformation of an 'Over' query is internally
-- well-typed, i.e. that its composed steps line up.
checkTransformation :: Query -> Either TypeError Query
checkTransformation q@(Over _ transformation) =
  case buildTransformationAST transformation of
    Left transformationTypeError -> Left $ InvalidTransformationType transformationTypeError
    Right _ -> pure q
checkTransformation q = pure q
