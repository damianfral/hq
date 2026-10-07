{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Transformation.ASTSpec (spec) where

import Data.Aeson (Value (..))
import HQ.Transformation
import HQ.Transformation.AST
import HQ.Transformation.TransformationType
import Relude hiding (Compose, Const, and, isPrefixOf, length, not, or, reverse, subtract, xor)
import Test.Syd

spec :: Spec
spec = describe "HQ.Transformation.AST" $ do
  describe "inferTransformationType" $ do
    it "tags a numeric step as number to number" $ do
      inferTransformationType (Add 1)
        `shouldBe` Right (TransformationType ValueNumber ValueNumber)

    it "tags less-than as number to boolean" $ do
      inferTransformationType (Lt 1)
        `shouldBe` Right (TransformationType ValueNumber ValueBool)

    it "tags less-than-or-equal as number to boolean" $ do
      inferTransformationType (Lte 1)
        `shouldBe` Right (TransformationType ValueNumber ValueBool)

    it "tags a string step as string to string" $ do
      inferTransformationType (ConcatString " x")
        `shouldBe` Right (TransformationType ValueString ValueString)

    it "tags stripPrefix as string to string" $ do
      inferTransformationType (StripPrefix "pre")
        `shouldBe` Right (TransformationType ValueString ValueString)

    it "tags stripSuffix as string to string" $ do
      inferTransformationType (StripSuffix "suf")
        `shouldBe` Right (TransformationType ValueString ValueString)

    it "tags isPrefixOf as string to boolean" $ do
      inferTransformationType (IsPrefixOf "pre")
        `shouldBe` Right (TransformationType ValueString ValueBool)

    it "tags isSuffixOf as string to boolean" $ do
      inferTransformationType (IsSuffixOf "suf")
        `shouldBe` Right (TransformationType ValueString ValueBool)

    it "tags isInfixOf as string to boolean" $ do
      inferTransformationType (IsInfixOf "fix")
        `shouldBe` Right (TransformationType ValueString ValueBool)

    it "tags isEmpty as array to boolean" $ do
      inferTransformationType IsEmpty
        `shouldBe` Right (TransformationType ValueArray ValueBool)

    it "tags length as array to number" $ do
      inferTransformationType ArrayLength
        `shouldBe` Right (TransformationType ValueArray ValueNumber)

    it "tags reverse as array to array" $ do
      inferTransformationType ArrayReverse
        `shouldBe` Right (TransformationType ValueArray ValueArray)

    it "tags unique as array to array" $ do
      inferTransformationType ArrayUnique
        `shouldBe` Right (TransformationType ValueArray ValueArray)

    it "tags sort as array to array" $ do
      inferTransformationType ArraySort
        `shouldBe` Right (TransformationType ValueArray ValueArray)

    it "tags a constant as any input to its own type" $ do
      inferTransformationType (Const (Number 3))
        `shouldBe` Right (TransformationType ValueAny ValueNumber)

    it "composes a constant before a step" $ do
      inferTransformationType (Compose (Const (Number 3)) (Add 1))
        `shouldBe` Right (TransformationType ValueNumber ValueNumber)

    it "composes a step before a constant" $ do
      inferTransformationType (Compose (Add 1) (Const (Number 3)))
        `shouldBe` Right (TransformationType ValueAny ValueNumber)

    it "rejects a step before a mismatched constant" $ do
      let expected =
            InvalidCompose
              (Compose (Add 1) (Const (String "x")))
              ValueString
              ValueNumber
      case inferTransformationType (Compose (Add 1) (Const (String "x"))) of
        Left err -> err `shouldBe` expected
        Right _ -> expectationFailure "expected an InvalidCompose"

    it "types a composition right to left" $ do
      inferTransformationType (Compose (Add 1) (Multiply 2))
        `shouldBe` Right (TransformationType ValueNumber ValueNumber)

    it "composes a boolean check inside a chain" $ do
      -- 'equal' accepts any input value, so the composition's input is Any.
      inferTransformationType (Compose Not (Equal (Number 1)))
        `shouldBe` Right (TransformationType ValueAny ValueBool)

    it "types or as boolean" $ do
      inferTransformationType (Or (Equal (Number 1)) (Equal (Number 2)))
        `shouldBe` Right (TransformationType ValueAny ValueBool)

    it "types and as boolean" $ do
      inferTransformationType (And (Equal (Number 1)) (Equal (Number 1)))
        `shouldBe` Right (TransformationType ValueAny ValueBool)

    it "types xor as boolean" $ do
      inferTransformationType (Xor (Equal (Number 1)) (Equal (Number 2)))
        `shouldBe` Right (TransformationType ValueAny ValueBool)

    it "accepts equal after a step of any type" $ do
      inferTransformationType (Compose (Equal (Number 1)) Trim)
        `shouldBe` Right (TransformationType ValueString ValueBool)

    it "rejects or with a non-boolean left branch" $ do
      let bad = Or (Add 1) (Equal (Number 2))
      case inferTransformationType bad of
        Left err -> err `shouldBe` InvalidOr bad ValueNumber
        Right _ -> expectationFailure "expected an InvalidOr"

    it "rejects or with a non-boolean right branch" $ do
      let bad = Or (Equal (Number 1)) (Add 2)
      case inferTransformationType bad of
        Left err -> err `shouldBe` InvalidOr bad ValueNumber
        Right _ -> expectationFailure "expected an InvalidOr"

    it "rejects and with a non-boolean left branch" $ do
      let bad = And (Add 1) (Equal (Number 2))
      case inferTransformationType bad of
        Left err -> err `shouldBe` InvalidAnd bad ValueNumber
        Right _ -> expectationFailure "expected an InvalidAnd"

    it "rejects xor with a non-boolean right branch" $ do
      let bad = Xor (Equal (Number 1)) (Add 2)
      case inferTransformationType bad of
        Left err -> err `shouldBe` InvalidXor bad ValueNumber
        Right _ -> expectationFailure "expected an InvalidXor"

    it "rejects a composition whose seam does not match" $ do
      let expected =
            InvalidCompose
              (Compose (Add 1) (Equal (Number 3)))
              ValueBool
              ValueNumber
      case inferTransformationType (Compose (Add 1) (Equal (Number 3))) of
        Left err -> err `shouldBe` expected
        Right _ -> expectationFailure "expected an InvalidCompose"

    it "rejects a composition of unrelated types" $ do
      let expected =
            InvalidCompose
              (Compose (ConcatString " x") (Add 1))
              ValueNumber
              ValueString
      case inferTransformationType (Compose (ConcatString " x") (Add 1)) of
        Left err -> err `shouldBe` expected
        Right _ -> expectationFailure "expected an InvalidCompose"

    it "rejects length before a string step" $ do
      let expected =
            InvalidCompose
              (Compose (StripPrefix "a") ArrayLength)
              ValueNumber
              ValueString
      case inferTransformationType (Compose (StripPrefix "a") ArrayLength) of
        Left err -> err `shouldBe` expected
        Right _ -> expectationFailure "expected an InvalidCompose"

    it "accepts a predicate after an array step" $ do
      inferTransformationType (Compose IsEmpty ArrayReverse)
        `shouldBe` Right (TransformationType ValueArray ValueBool)

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
