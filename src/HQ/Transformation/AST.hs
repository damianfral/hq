{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Transformation.AST where

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
newtype TransformationAST = TransformationAST {unTransformationAST :: Cofree TransformationF TransformationType}

-- | The type error produced when the composed steps of a transformation do
-- not line up.
data TransformationTypeError
  = -- | @InvalidCombine transformation out in@: the output type of the right
    -- step of a 'Combine' does not match the input type of the left step.
    -- The offending 'Combine' is kept so callers can report it to the user.
    InvalidCombine Transformation ValueType ValueType
  deriving (Eq, Show)

-- | Build the type-annotated AST of a transformation, failing when the steps
-- of a 'Combine' do not line up: the output type of the right step must
-- match the input type of the left step, unless the left step accepts any
-- value (a constant).
buildTransformationAST :: Transformation -> Either TransformationTypeError TransformationAST
buildTransformationAST (Transformation transformation) =
  TransformationAST <$> foldFix algebra transformation
  where
    algebra :: TransformationF (Either TransformationTypeError (Cofree TransformationF TransformationType)) -> Either TransformationTypeError (Cofree TransformationF TransformationType)
    algebra (Add n) = pure $ TransformationType ValueNumber ValueNumber :< Add n
    algebra (Multiply n) = pure $ TransformationType ValueNumber ValueNumber :< Multiply n
    algebra (Subtract n) = pure $ TransformationType ValueNumber ValueNumber :< Subtract n
    algebra (Divide n) = pure $ TransformationType ValueNumber ValueNumber :< Divide n
    algebra (ConcatString s) = pure $ TransformationType ValueString ValueString :< ConcatString s
    algebra (ConcatArray xs) = pure $ TransformationType ValueArray ValueArray :< ConcatArray xs
    algebra Trim = pure $ TransformationType ValueString ValueString :< Trim
    algebra (Replace a b) = pure $ TransformationType ValueString ValueString :< Replace a b
    algebra (Equal v) = pure $ TransformationType (valueType v) ValueBool :< Equal v
    algebra (Const v) = pure $ TransformationType ValueAny (valueType v) :< Const v
    algebra Not = pure $ TransformationType ValueBool ValueBool :< Not
    algebra (Or left right) = do
      l <- left
      r <- right
      pure $ TransformationType (transformationInput (view _extract l)) ValueBool :< Or l r
    algebra (Combine left right) = do
      l <- left
      r <- right
      let lt = view _extract l
          rt = view _extract r
      if transformationInput lt == ValueAny || transformationOutput rt == transformationInput lt
        then pure $ TransformationType (transformationInput rt) (transformationOutput lt) :< Combine l r
        else Left $ InvalidCombine (subTransformation (Combine l r)) (transformationOutput rt) (transformationInput lt)

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
subTransformation :: TransformationF (Cofree TransformationF TransformationType) -> Transformation
subTransformation = Transformation . Fix . fmap cofreeToFix

-- | The root type of a well-typed transformation.
rootType :: Transformation -> Either TransformationTypeError TransformationType
rootType t = do
  TransformationAST (tt :< _) <- buildTransformationAST t
  pure tt
