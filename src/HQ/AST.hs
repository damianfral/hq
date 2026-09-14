{-# LANGUAGE NoImplicitPrelude #-}

module HQ.AST where

import Control.Comonad.Cofree (Cofree ((:<)), _extract)
import HQ.Optic
import Relude hiding (Compose, id)
import Relude.Extra (view)

-- | OpticType of an optic: whether it focuses on one value or many.
data OpticType = OpticLens | OpticPrism | OpticAffineTraversal | OpticTraversal
  deriving (Eq, Show)

{-
          | Lens      | Prism     | Affine    | Traversal
----------+-----------+-----------+-----------+----------
Lens      | Lens      | Prism     | Affine    | Traversal
Prism     | Prism     | Prism     | Affine    | Traversal
Affine    | Affine    | Affine    | Affine    | Traversal
Traversal | Traversal | Traversal | Traversal | Traversal
-}

-- | OpticType combines via the rule: One + One = One, else Many.
-- This corresponds to the composition of optics: composing two
-- single-target optics yields a single-target optic.
instance Semigroup OpticType where
  OpticTraversal <> _ = OpticTraversal
  _ <> OpticTraversal = OpticTraversal
  OpticLens <> OpticLens = OpticLens
  OpticLens <> OpticPrism = OpticPrism
  OpticPrism <> OpticLens = OpticPrism
  OpticLens <> OpticAffineTraversal = OpticAffineTraversal
  OpticAffineTraversal <> OpticLens = OpticAffineTraversal
  OpticPrism <> OpticPrism = OpticPrism
  OpticPrism <> OpticAffineTraversal = OpticAffineTraversal
  OpticAffineTraversal <> OpticPrism = OpticAffineTraversal
  OpticAffineTraversal <> OpticAffineTraversal = OpticAffineTraversal

instance Monoid OpticType where mempty = OpticLens

-- | An optic annotated with cardinality information at each node.
-- Used for static validation that optic compositions are well-typed.
newtype OpticAST = OpticAST {unOpticAST :: Cofree OpticF OpticType}

-- | Errors that can occur during optic typechecking.
data TypeError = InvalidOpticType OpticType OpticType deriving (Eq, Show)

-- | Check that an annotated optic tree has consistent cardinality (OpticType).
typecheck :: OpticAST -> Either TypeError Optic
typecheck = go . unOpticAST
  where
    go :: Cofree OpticF OpticType -> Either TypeError Optic
    go (ann :< op) = case op of
      Field name
        | ann == OpticAffineTraversal -> pure $ field name
        | otherwise -> Left $ InvalidOpticType OpticAffineTraversal ann
      Each
        | ann == OpticTraversal -> pure each
        | otherwise -> Left $ InvalidOpticType OpticTraversal ann
      Id
        | ann == OpticLens -> pure id
        | otherwise -> Left $ InvalidOpticType OpticLens ann
      PrismString
        | ann == OpticPrism -> pure _String
        | otherwise -> Left $ InvalidOpticType OpticPrism ann
      PrismNumber
        | ann == OpticPrism -> pure _Number
        | otherwise -> Left $ InvalidOpticType OpticPrism ann
      PrismBool
        | ann == OpticPrism -> pure _Bool
        | otherwise -> Left $ InvalidOpticType OpticPrism ann
      PrismNull
        | ann == OpticPrism -> pure _Null
        | otherwise -> Left $ InvalidOpticType OpticPrism ann
      PrismArray
        | ann == OpticPrism -> pure _Array
        | otherwise -> Left $ InvalidOpticType OpticPrism ann
      PrismObject
        | ann == OpticPrism -> pure _Object
        | otherwise -> Left $ InvalidOpticType OpticPrism ann
      PrismJust
        | ann == OpticPrism -> pure _Just
        | otherwise -> Left $ InvalidOpticType OpticPrism ann
      Prism1
        | ann == OpticPrism -> pure _1
        | otherwise -> Left $ InvalidOpticType OpticPrism ann
      Prism2
        | ann == OpticPrism -> pure _2
        | otherwise -> Left $ InvalidOpticType OpticPrism ann
      Compose left right -> do
        l <- go left
        r <- go right
        let lc = view _extract left
            rc = view _extract right
            expected = lc <> rc
        if ann == expected
          then pure $ compose l r
          else Left $ InvalidOpticType expected ann
