{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Optic.OpticType where

import Relude hiding (Compose, id)

-- | OpticType
--
-- +-----------+-----------+-----------+-----------+-----------+
-- |           | Lens      | Prism     | Affine    | Traversal |
-- +===========+===========+===========+===========+===========+
-- | Lens      | Lens      | Prism     | Affine    | Traversal |
-- +-----------+-----------+-----------+-----------+-----------+
-- | Prism     | Prism     | Prism     | Affine    | Traversal |
-- +-----------+-----------+-----------+-----------+-----------+
-- | Affine    | Affine    | Affine    | Affine    | Traversal |
-- +-----------+-----------+-----------+-----------+-----------+
-- | Traversal | Traversal | Traversal | Traversal | Traversal |
-- +-----------+-----------+-----------+-----------+-----------+
data OpticType = OpticLens | OpticPrism | OpticAffineTraversal | OpticTraversal
  deriving (Eq, Ord, Show, Bounded)

-- | Composition lattice: composing takes the 'max'.
-- Constructor order is the lattice order, so composition is 'max'.
instance Semigroup OpticType where (<>) = max

instance Monoid OpticType where mempty = minBound
