{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Optic.AST where

import Control.Comonad.Cofree (Cofree ((:<)), _extract)
import Data.Fix (foldFix)
import HQ.Optic
import HQ.Optic.OpticType
import Relude hiding (Compose, id)
import Relude.Extra (view)

-- | An optic annotated with cardinality information at each node.
-- Used for static validation that optic compositions are well-typed.
newtype OpticAST = OpticAST {unOpticAST :: Cofree OpticF OpticType}

buildOpticAST :: Optic -> OpticAST
buildOpticAST (Optic optic) = OpticAST $ foldFix algebra optic
  where
    algebra o@(Field _) = OpticAffineTraversal :< o
    algebra o@Each = OpticTraversal :< o
    algebra o@Keys = OpticTraversal :< o
    algebra o@Values = OpticTraversal :< o
    algebra o@Id = OpticLens :< o
    algebra o@(Compose left right) = do
      let l = view _extract left
          r = view _extract right
      l <> r :< o
    algebra o@(Ix _) = OpticAffineTraversal :< o
    -- '_Just' is matching-only (its identity 'review' is not a section
    -- on 'Null'), so it is an affine traversal rather than a prism.
    algebra o@PrismJust = OpticAffineTraversal :< o
    -- 'Filter' keeps its input zero or one times; the annotation of its
    -- sub-optic is irrelevant to its own cardinality.
    algebra o@(Filter _ _) = OpticAffineTraversal :< o
    algebra p = OpticPrism :< p
