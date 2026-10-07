{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Optic.AST where

import HQ.Optic
import HQ.Optic.OpticType
import HQ.Transformation.TransformationType (ValueType (..))
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

-- | Infer the value type an optic focuses. Prisms (and 'Keys') pin
-- the type down; 'Field', 'Each', 'Values', 'Ix' and 'Id' focus
-- values of unknown shape, and 'Filter' keeps its input, so all of
-- those yield 'ValueAny'. Composition takes the right side: the
-- result values are whatever the rightmost selector focuses. Total,
-- and sound: a focus is always of the inferred type or nothing, so
-- only exact (non-'Any') mismatches are ever reported.
inferFocusType :: Optic -> ValueType
inferFocusType = go
  where
    go (Field _) = ValueAny
    go Each = ValueAny
    go Keys = ValueString
    go Values = ValueAny
    go Id = ValueAny
    go (Compose _ right) = go right
    go (Ix _) = ValueAny
    go PrismJust = ValueAny
    go (Filter _ _) = ValueAny
    go (Prism PString) = ValueString
    go (Prism PNumber) = ValueNumber
    go (Prism PBool) = ValueBool
    go (Prism PNull) = ValueNull
    go (Prism PArray) = ValueArray
    go (Prism PObject) = ValueObject
