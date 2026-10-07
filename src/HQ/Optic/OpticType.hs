{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Optic.OpticType where

import Relude hiding (Compose, id)

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

-- | Composition lattice: One + One = One, else Many (see table above).
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

-- | Subsumption: everything folds, so traversals accept anything.
-- (All four queries accept traversals; 'Preview' emits the first focus.)
canUseAs :: OpticType -> OpticType -> Bool
canUseAs OpticTraversal _ = True
canUseAs OpticPrism actual = actual /= OpticTraversal
canUseAs OpticAffineTraversal actual = actual /= OpticTraversal
canUseAs OpticLens actual = actual == OpticLens
