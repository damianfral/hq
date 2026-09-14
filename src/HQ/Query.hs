{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Query where

import Control.Comonad.Cofree (Cofree ((:<)))
import HQ.AST
import HQ.Optic
import HQ.Optic.OpticType (OpticType (..))
import HQ.Value (Value)
import Relude hiding (Compose, many, some)

--------------------------------------------------------------------------------

data Query
  = Fold Optic
  | Preview Optic
  | Set Optic Value
  | Delete Optic
  deriving (Show, Eq)

getOptic :: Query -> Optic
getOptic (Fold optic) = optic
getOptic (Preview optic) = optic
getOptic (Set optic _) = optic
getOptic (Delete optic) = optic

matchOpticType :: Query -> OpticType
matchOpticType (Fold _) = OpticTraversal
matchOpticType (Preview _) = OpticPrism
matchOpticType (Set _ _) = OpticTraversal
matchOpticType (Delete _) = OpticTraversal

data TypeError = InvalidOpticType OpticType OpticType deriving (Eq, Show)

typecheckQuery :: Query -> Either TypeError Query
typecheckQuery q = if expected == current then pure q else Left err
  where
    err = InvalidOpticType expected current
    expected = matchOpticType q
    OpticAST (current :< _) = buildOpticAST (getOptic q)
