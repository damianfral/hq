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
    algebra o@(Field _) = OpticLens :< o
    algebra o@Each = OpticTraversal :< o
    algebra o@Id = OpticLens :< o
    algebra o@(Compose left right) = do
      let l = view _extract left
          r = view _extract right
      l <> r :< o
    algebra p = OpticPrism :< p
