{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Optic.OpticType where

import Relude hiding (Compose, id)

data OpticType = OpticLens | OpticPrism | OpticAffineTraversal | OpticTraversal
  deriving (Eq, Ord, Show, Enum, Bounded)

{-
          | Lens      | Prism     | Affine    | Traversal
----------+-----------+-----------+-----------+----------
Lens      | Lens      | Prism     | Affine    | Traversal
Prism     | Prism     | Prism     | Affine    | Traversal
Affine    | Affine    | Affine    | Affine    | Traversal
Traversal | Traversal | Traversal | Traversal | Traversal
-}

-- | Composition lattice: One + One = One, else Many (see table above).
-- Constructor order is the lattice order, so composition is 'max'.
instance Semigroup OpticType where
  (<>) = max

instance Monoid OpticType where mempty = minBound

-- | Subsumption: @canUseAs expected actual@ holds when @actual@ is no
-- more general than @expected@. A traversal accepts anything; an
-- affine accepts lens/prism/affine; a prism accepts lens/prism; a
-- lens accepts only a lens. In particular a prism can be used where
-- an affine is expected, but not vice versa.
canUseAs :: OpticType -> OpticType -> Bool
canUseAs expected actual = actual <= expected
