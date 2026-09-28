{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Transformation.AST
  ( buildTransformationAST,
    TransformationAST (..),
    TransformationType (..),
    TransformationTypeError (..),
    rootType,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)), _extract)
import Data.Aeson.Types (Value (..))
import Data.Fix (Fix (..), foldFix)
import HQ.Transformation
import HQ.Transformation.TransformationType
import Relude hiding (Const, many, not, or, some, subtract, toStrict)
import Relude.Extra (view)

-- | A transformation annotated with input and output value types at each
-- node. Used for static validation that transformation compositions are
-- well-typed.
newtype TransformationAST = TransformationAST {unTransformationAST :: AST}

type AST = Cofree TransformationF TransformationType

-- | The type error produced when the composed steps of a transformation do
-- not line up.
data TransformationTypeError
  = -- | @InvalidCombine transformation out in@: the output type of the right
    -- step of a 'Combine' does not match the input type of the left step.
    -- The offending 'Combine' is kept so callers can report it to the user.
    InvalidCombine Transformation ValueType ValueType
  | -- | @InvalidOr transformation out@: a branch of an 'Or' does not
    -- produce a boolean, so the disjunction is ill-typed. The offending
    -- 'Or' is kept so callers can report it to the user.
    InvalidOr Transformation ValueType
  | -- | @InvalidFilter transformation out@: a @filter@ predicate does not
    -- produce a boolean. The offending transformation is kept so callers
    -- can report it to the user.
    InvalidFilter Transformation ValueType
  deriving (Eq, Show)

-- | Build the type-annotated AST of a transformation, failing when the steps
-- of a 'Combine' do not line up: the output type of the right step must
-- match the input type of the left step, unless the left step accepts any
-- value (a constant).
buildTransformationAST ::
  Transformation -> Either TransformationTypeError TransformationAST
buildTransformationAST (Transformation transformation) =
  TransformationAST <$> foldFix algebra transformation
  where
    algebra ::
      TransformationF (Either TransformationTypeError AST) ->
      Either TransformationTypeError AST
    algebra (Add n) = pure $ TransformationType ValueNumber ValueNumber :< Add n
    algebra (Multiply n) =
      pure $ TransformationType ValueNumber ValueNumber :< Multiply n
    algebra (Subtract n) =
      pure $ TransformationType ValueNumber ValueNumber :< Subtract n
    algebra (Divide n) =
      pure $ TransformationType ValueNumber ValueNumber :< Divide n
    algebra (ConcatString s) =
      pure $ TransformationType ValueString ValueString :< ConcatString s
    algebra (ConcatArray xs) =
      pure $ TransformationType ValueArray ValueArray :< ConcatArray xs
    algebra Trim = pure $ TransformationType ValueString ValueString :< Trim
    algebra (Replace a b) =
      pure $ TransformationType ValueString ValueString :< Replace a b
    algebra (Equal v) = pure $ TransformationType ValueAny ValueBool :< Equal v
    algebra (Const v) =
      pure $ TransformationType ValueAny (valueType v) :< Const v
    algebra Not = pure $ TransformationType ValueBool ValueBool :< Not
    algebra (Or left right) = do
      l <- left
      r <- right
      let lt = view _extract l
          rt = view _extract r
      case (transformationOutput lt, transformationOutput rt) of
        (ValueBool, ValueBool) ->
          let inputType = transformationInput $ view _extract l
           in pure $ TransformationType inputType ValueBool :< Or l r
        (ValueBool, other) ->
          Left $ InvalidOr (subTransformation (Or l r)) other
        (other, _) ->
          Left $ InvalidOr (subTransformation (Or l r)) other
    algebra (Combine left right) = do
      l <- left
      r <- right
      let lt = view _extract l
          rt = view _extract r
          conditions =
            [ transformationInput lt == ValueAny,
              transformationOutput rt == transformationInput lt
            ]
          inputType = transformationInput rt
          outputType = transformationOutput lt
      if getAny $ foldMap Any conditions
        then do
          pure $ TransformationType inputType outputType :< Combine l r
        else
          Left
            $ InvalidCombine
              (subTransformation (Combine l r))
              (transformationOutput rt)
              (transformationInput lt)

-- | The 'ValueType' of an embedded JSON literal.
valueType :: Value -> ValueType
valueType (Object _) = ValueObject
valueType (Array _) = ValueArray
valueType (String _) = ValueString
valueType (Number _) = ValueNumber
valueType (Bool _) = ValueBool
valueType Null = ValueNull

-- | Drop the annotations from a 'Cofree' to recover the underlying tree.
cofreeToFix :: (Functor f) => Cofree f a -> Fix f
cofreeToFix (_ :< f) = Fix (fmap cofreeToFix f)

-- | The original transformation represented by an annotated subtree.
subTransformation :: TransformationF AST -> Transformation
subTransformation = Transformation . Fix . fmap cofreeToFix

-- | The root type of a well-typed transformation.
rootType :: Transformation -> Either TransformationTypeError TransformationType
rootType t = do
  TransformationAST (tt :< _) <- buildTransformationAST t
  pure tt
