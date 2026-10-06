{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Optic.AST where

import HQ.Optic
import HQ.Optic.OpticType
import Relude hiding (Compose, id)

-- | Infer an optic's cardinality ('OpticType' lattice, combined with
-- '<>' over compositions). Total: every optic has a cardinality.
inferOpticType :: Optic -> OpticType
inferOpticType = go
  where
    go (Field _) = OpticAffineTraversal
    go Each = OpticTraversal
    go Keys = OpticTraversal
    go Values = OpticTraversal
    go Id = OpticLens
    go (Compose left right) = go left <> go right
    go (Ix _) = OpticAffineTraversal
    -- '_Just' is matching-only (its identity 'review' is not a section
    -- on 'Null'), so it is an affine traversal rather than a prism.
    go PrismJust = OpticAffineTraversal
    -- 'Filter' keeps its input zero or one times; the annotation of its
    -- sub-optic is irrelevant to its own cardinality.
    go (Filter _ _) = OpticAffineTraversal
    go (Prism _) = OpticPrism
