{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Transformation.ASTSpec (spec) where

import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import HQ.Transformation
import HQ.Transformation.AST
import HQ.Transformation.TransformationType
import Relude hiding (Compose, Const, and, isPrefixOf, length, not, or, reverse, subtract, xor)
import Test.Syd

spec :: Spec
spec = describe "HQ.Transformation.AST" $ do
  describe "inferTransformationType" $ do
    it "tags well-typed transformations" $ forM_ happyCases $ \(t, expected) ->
      inferTransformationType t `shouldBe` Right expected

    it "rejects ill-typed transformations" $ forM_ errorCases $ \(t, expected) ->
      inferTransformationType t `shouldBe` Left expected

    it "reports the failing composition in DSL syntax" $ do
      case inferTransformationType (Compose (Add 1) (Equal (Number 3))) of
        Left (InvalidCompose t _ _) -> show t `shouldBe` ("+1 . == 3" :: String)
        Left err -> expectationFailure $ "wrong error: " <> show err
        Right _ -> expectationFailure "expected an InvalidCompose"

    it "renders and/xor precedence without redundant parens" $ do
      show (Or (And (Equal (Number 1)) (Equal (Number 1))) (Equal (Number 3)))
        `shouldBe` ("== 1 and == 1 or == 3" :: String)
      show (And (Equal (Number 1)) (Or (Equal (Number 2)) (Equal (Number 3))))
        `shouldBe` ("== 1 and (== 2 or == 3)" :: String)
      show (Xor (And (Equal (Number 1)) (Equal (Number 2))) (Equal (Number 3)))
        `shouldBe` ("== 1 and == 2 xor == 3" :: String)

happyCases :: [(Transformation, TransformationType)]
happyCases =
  [ (Add 1, TransformationType ValueNumber ValueNumber),
    (Lt 1, TransformationType ValueNumber ValueBool),
    (Lte 1, TransformationType ValueNumber ValueBool),
    (ConcatString " x", TransformationType ValueString ValueString),
    (StripPrefix "pre", TransformationType ValueString ValueString),
    (StripSuffix "suf", TransformationType ValueString ValueString),
    (IsPrefixOf "pre", TransformationType ValueString ValueBool),
    (IsSuffixOf "suf", TransformationType ValueString ValueBool),
    (IsInfixOf "fix", TransformationType ValueString ValueBool),
    (IsEmpty, TransformationType ValueArray ValueBool),
    (ArrayLength, TransformationType ValueArray ValueNumber),
    (ArrayReverse, TransformationType ValueArray ValueArray),
    (ArrayUnique, TransformationType ValueArray ValueArray),
    (ArraySort, TransformationType ValueArray ValueArray),
    (Merge (KeyMap.fromList [("b", Number 1)]), TransformationType ValueObject ValueObject),
    (DeepMerge (KeyMap.fromList [("b", Number 1)]), TransformationType ValueObject ValueObject),
    (Const (Number 3), TransformationType ValueAny ValueNumber),
    (Compose (Const (Number 3)) (Add 1), TransformationType ValueNumber ValueNumber),
    (Compose (Add 1) (Const (Number 3)), TransformationType ValueAny ValueNumber),
    (Compose (Add 1) (Multiply 2), TransformationType ValueNumber ValueNumber),
    (Compose Not (Equal (Number 1)), TransformationType ValueAny ValueBool),
    (Or (Equal (Number 1)) (Equal (Number 2)), TransformationType ValueAny ValueBool),
    (And (Equal (Number 1)) (Equal (Number 1)), TransformationType ValueAny ValueBool),
    (Xor (Equal (Number 1)) (Equal (Number 2)), TransformationType ValueAny ValueBool),
    (Compose (Equal (Number 1)) Trim, TransformationType ValueString ValueBool),
    (Compose IsEmpty ArrayReverse, TransformationType ValueArray ValueBool)
  ]

errorCases :: [(Transformation, TransformationTypeError)]
errorCases =
  [ (Divide 0, InvalidDivideByZero (Divide 0)),
    (Compose (Add 1) (Const (String "x")), InvalidCompose (Compose (Add 1) (Const (String "x"))) ValueString ValueNumber),
    (Or (Add 1) (Equal (Number 2)), InvalidOr (Or (Add 1) (Equal (Number 2))) ValueNumber),
    (Or (Equal (Number 1)) (Add 2), InvalidOr (Or (Equal (Number 1)) (Add 2)) ValueNumber),
    (And (Add 1) (Equal (Number 2)), InvalidAnd (And (Add 1) (Equal (Number 2))) ValueNumber),
    (Xor (Equal (Number 1)) (Add 2), InvalidXor (Xor (Equal (Number 1)) (Add 2)) ValueNumber),
    (Compose (Add 1) (Equal (Number 3)), InvalidCompose (Compose (Add 1) (Equal (Number 3))) ValueBool ValueNumber),
    (Compose (ConcatString " x") (Add 1), InvalidCompose (Compose (ConcatString " x") (Add 1)) ValueNumber ValueString),
    (Compose (StripPrefix "a") ArrayLength, InvalidCompose (Compose (StripPrefix "a") ArrayLength) ValueNumber ValueString)
  ]
