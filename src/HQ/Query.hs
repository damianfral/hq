{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Query where

import Control.Comonad.Cofree (Cofree ((:<)))
import HQ.JSON.Event (JSONEvent)
import HQ.Optic
import HQ.Optic.AST
import HQ.Optic.OpticType (OpticType (..))
import HQ.Transformation
import HQ.Transformation.AST
import Relude hiding (Compose, many, not, or, some, subtract)

--------------------------------------------------------------------------------

data Query
  = Fold Optic
  | Preview Optic
  | Set Optic [JSONEvent]
  | -- | Apply a transformation to each of the focused values.
    Over Optic Transformation
  | Delete Optic
  deriving (Show, Eq)

getOptic :: Query -> Optic
getOptic (Fold optic) = optic
getOptic (Preview optic) = optic
getOptic (Over optic _) = optic
getOptic (Set optic _) = optic
getOptic (Delete optic) = optic

-- | The optic type a query requires: how many focus points it expects.
-- 'typecheckQuery' compares this against the type derived from the
-- optic itself.
queryOpticType :: Query -> OpticType
queryOpticType (Fold _) = OpticTraversal
queryOpticType (Preview _) = OpticPrism
queryOpticType (Set _ _) = OpticTraversal
queryOpticType (Over _ _) = OpticTraversal
queryOpticType (Delete _) = OpticTraversal

data TypeError
  = InvalidOpticType OpticType OpticType
  | InvalidTransformationType TransformationTypeError
  deriving (Eq, Show)

typecheckQuery :: Query -> Either TypeError Query
typecheckQuery q =
  if expected == current
    then checkTransformation q
    else Left err
  where
    err = InvalidOpticType expected current
    expected = queryOpticType q
    OpticAST (current :< _) = buildOpticAST (getOptic q)

-- | Check that the transformation of an 'Over' query is internally
-- well-typed, i.e. that its composed steps line up.
checkTransformation :: Query -> Either TypeError Query
checkTransformation q@(Over _ transformation) =
  case buildTransformationAST transformation of
    Left transformationTypeError -> Left $ InvalidTransformationType transformationTypeError
    Right _ -> pure q
checkTransformation q = pure q
