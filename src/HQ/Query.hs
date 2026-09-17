{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Query where

import Control.Comonad.Cofree (Cofree ((:<)))
import HQ.JSON.Event (JSONEvent)
import HQ.Optic
import HQ.Optic.AST
import HQ.Optic.OpticType (OpticType (..))
import Relude hiding (Compose, many, some)

--------------------------------------------------------------------------------

data Query
  = Fold Optic
  | Preview Optic
  | Set Optic [JSONEvent]
  | Delete Optic
  deriving (Show, Eq)

getOptic :: Query -> Optic
getOptic (Fold optic) = optic
getOptic (Preview optic) = optic
getOptic (Set optic _) = optic
getOptic (Delete optic) = optic

-- | The optic type a query requires: how many focus points it expects.
-- 'typecheckQuery' compares this against the type derived from the
-- optic itself.
queryOpticType :: Query -> OpticType
queryOpticType (Fold _) = OpticTraversal
queryOpticType (Preview _) = OpticPrism
queryOpticType (Set _ _) = OpticTraversal
queryOpticType (Delete _) = OpticTraversal

data TypeError = InvalidOpticType OpticType OpticType deriving (Eq, Show)

typecheckQuery :: Query -> Either TypeError Query
typecheckQuery q = if expected == current then pure q else Left err
  where
    err = InvalidOpticType expected current
    expected = queryOpticType q
    OpticAST (current :< _) = buildOpticAST (getOptic q)
