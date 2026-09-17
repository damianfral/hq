{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Optic.OpticType where

import Relude hiding (Compose, id)

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
