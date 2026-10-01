{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Transformation.ASTSpec (spec) where

import Data.Aeson (Value (..))
import HQ.Transformation
import HQ.Transformation.AST
import HQ.Transformation.TransformationType
import Relude hiding (and, isPrefixOf, length, not, or, reverse, subtract, xor)
import Test.Syd

spec :: Spec
spec = describe "HQ.Transformation.AST" $ do
  describe "buildTransformationAST" $ do
    it "tags a numeric step as number to number" $ do
      rootType (add 1)
        `shouldBe` Right (TransformationType ValueNumber ValueNumber)

    it "tags a string step as string to string" $ do
      rootType (concatString " x")
        `shouldBe` Right (TransformationType ValueString ValueString)

    it "tags stripPrefix as string to string" $ do
      rootType (stripPrefix "pre")
        `shouldBe` Right (TransformationType ValueString ValueString)

    it "tags stripSuffix as string to string" $ do
      rootType (stripSuffix "suf")
        `shouldBe` Right (TransformationType ValueString ValueString)

    it "tags isPrefixOf as string to boolean" $ do
      rootType (isPrefixOf "pre")
        `shouldBe` Right (TransformationType ValueString ValueBool)

    it "tags isSuffixOf as string to boolean" $ do
      rootType (isSuffixOf "suf")
        `shouldBe` Right (TransformationType ValueString ValueBool)

    it "tags isInfixOf as string to boolean" $ do
      rootType (isInfixOf "fix")
        `shouldBe` Right (TransformationType ValueString ValueBool)

    it "tags isEmpty as array to boolean" $ do
      rootType isEmpty
        `shouldBe` Right (TransformationType ValueArray ValueBool)

    it "tags length as array to number" $ do
      rootType length
        `shouldBe` Right (TransformationType ValueArray ValueNumber)

    it "tags reverse as array to array" $ do
      rootType reverse
        `shouldBe` Right (TransformationType ValueArray ValueArray)

    it "tags unique as array to array" $ do
      rootType unique
        `shouldBe` Right (TransformationType ValueArray ValueArray)

    it "tags a constant as any input to its own type" $ do
      rootType (constValue (Number 3))
        `shouldBe` Right (TransformationType ValueAny ValueNumber)

    it "composes a constant before a step" $ do
      rootType (compose (constValue (Number 3)) (add 1))
        `shouldBe` Right (TransformationType ValueNumber ValueNumber)

    it "composes a step before a constant" $ do
      rootType (compose (add 1) (constValue (Number 3)))
        `shouldBe` Right (TransformationType ValueAny ValueNumber)

    it "rejects a step before a mismatched constant" $ do
      let expected =
            InvalidCompose
              (compose (add 1) (constValue (String "x")))
              ValueString
              ValueNumber
      case buildTransformationAST (compose (add 1) (constValue (String "x"))) of
        Left err -> err `shouldBe` expected
        Right _ -> expectationFailure "expected an InvalidCompose"

    it "types a composition right to left" $ do
      rootType (compose (add 1) (multiply 2))
        `shouldBe` Right (TransformationType ValueNumber ValueNumber)

    it "composes a boolean check inside a chain" $ do
      -- 'equal' accepts any input value, so the composition's input is Any.
      rootType (compose not (equal (Number 1)))
        `shouldBe` Right (TransformationType ValueAny ValueBool)

    it "types or as boolean" $ do
      rootType (or (equal (Number 1)) (equal (Number 2)))
        `shouldBe` Right (TransformationType ValueAny ValueBool)

    it "types and as boolean" $ do
      rootType (and (equal (Number 1)) (equal (Number 1)))
        `shouldBe` Right (TransformationType ValueAny ValueBool)

    it "types xor as boolean" $ do
      rootType (xor (equal (Number 1)) (equal (Number 2)))
        `shouldBe` Right (TransformationType ValueAny ValueBool)

    it "accepts equal after a step of any type" $ do
      rootType (compose (equal (Number 1)) trim)
        `shouldBe` Right (TransformationType ValueString ValueBool)

    it "rejects or with a non-boolean left branch" $ do
      let bad = or (add 1) (equal (Number 2))
      case buildTransformationAST bad of
        Left err -> err `shouldBe` InvalidOr bad ValueNumber
        Right _ -> expectationFailure "expected an InvalidOr"

    it "rejects or with a non-boolean right branch" $ do
      let bad = or (equal (Number 1)) (add 2)
      case buildTransformationAST bad of
        Left err -> err `shouldBe` InvalidOr bad ValueNumber
        Right _ -> expectationFailure "expected an InvalidOr"

    it "rejects and with a non-boolean left branch" $ do
      let bad = and (add 1) (equal (Number 2))
      case buildTransformationAST bad of
        Left err -> err `shouldBe` InvalidAnd bad ValueNumber
        Right _ -> expectationFailure "expected an InvalidAnd"

    it "rejects xor with a non-boolean right branch" $ do
      let bad = xor (equal (Number 1)) (add 2)
      case buildTransformationAST bad of
        Left err -> err `shouldBe` InvalidXor bad ValueNumber
        Right _ -> expectationFailure "expected an InvalidXor"

    it "rejects a composition whose seam does not match" $ do
      let expected =
            InvalidCompose
              (compose (add 1) (equal (Number 3)))
              ValueBool
              ValueNumber
      case buildTransformationAST (compose (add 1) (equal (Number 3))) of
        Left err -> err `shouldBe` expected
        Right _ -> expectationFailure "expected an InvalidCompose"

    it "rejects a composition of unrelated types" $ do
      let expected =
            InvalidCompose
              (compose (concatString " x") (add 1))
              ValueNumber
              ValueString
      case buildTransformationAST (compose (concatString " x") (add 1)) of
        Left err -> err `shouldBe` expected
        Right _ -> expectationFailure "expected an InvalidCompose"

    it "rejects length before a string step" $ do
      let expected =
            InvalidCompose
              (compose (stripPrefix "a") length)
              ValueNumber
              ValueString
      case buildTransformationAST (compose (stripPrefix "a") length) of
        Left err -> err `shouldBe` expected
        Right _ -> expectationFailure "expected an InvalidCompose"

    it "accepts a predicate after an array step" $ do
      rootType (compose isEmpty reverse)
        `shouldBe` Right (TransformationType ValueArray ValueBool)

    it "reports the failing composition in DSL syntax" $ do
      case buildTransformationAST (compose (add 1) (equal (Number 3))) of
        Left (InvalidCompose t _ _) -> show t `shouldBe` ("+1 . == 3" :: String)
        Left err -> expectationFailure $ "wrong error: " <> show err
        Right _ -> expectationFailure "expected an InvalidCompose"

    it "renders and/xor precedence without redundant parens" $ do
      show (or (and (equal (Number 1)) (equal (Number 1))) (equal (Number 3)))
        `shouldBe` ("== 1 and == 1 or == 3" :: String)
      show (and (equal (Number 1)) (or (equal (Number 2)) (equal (Number 3))))
        `shouldBe` ("== 1 and (== 2 or == 3)" :: String)
      show (xor (and (equal (Number 1)) (equal (Number 2))) (equal (Number 3)))
        `shouldBe` ("== 1 and == 2 xor == 3" :: String)
