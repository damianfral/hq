{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Transformation.ASTSpec (spec) where

import Data.Aeson (Value (..))
import HQ.Transformation
import HQ.Transformation.AST
import HQ.Transformation.TransformationType
import Relude hiding (not, or, subtract)
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

    it "tags a constant as any input to its own type" $ do
      rootType (constValue (Number 3))
        `shouldBe` Right (TransformationType ValueAny ValueNumber)

    it "composes a constant before a step" $ do
      rootType (combine (constValue (Number 3)) (add 1))
        `shouldBe` Right (TransformationType ValueNumber ValueNumber)

    it "composes a step before a constant" $ do
      rootType (combine (add 1) (constValue (Number 3)))
        `shouldBe` Right (TransformationType ValueAny ValueNumber)

    it "rejects a step before a mismatched constant" $ do
      let expected =
            InvalidCombine
              (combine (add 1) (constValue (String "x")))
              ValueString
              ValueNumber
      case buildTransformationAST (combine (add 1) (constValue (String "x"))) of
        Left err -> err `shouldBe` expected
        Right _ -> expectationFailure "expected an InvalidCombine"

    it "types a composition right to left" $ do
      rootType (combine (add 1) (multiply 2))
        `shouldBe` Right (TransformationType ValueNumber ValueNumber)

    it "composes a boolean check inside a chain" $ do
      rootType (combine not (equal (Number 1)))
        `shouldBe` Right (TransformationType ValueNumber ValueBool)

    it "types or as boolean" $ do
      rootType (or (equal (Number 1)) (equal (Number 2)))
        `shouldBe` Right (TransformationType ValueNumber ValueBool)

    it "rejects a composition whose seam does not match" $ do
      let expected =
            InvalidCombine
              (combine (add 1) (equal (Number 3)))
              ValueBool
              ValueNumber
      case buildTransformationAST (combine (add 1) (equal (Number 3))) of
        Left err -> err `shouldBe` expected
        Right _ -> expectationFailure "expected an InvalidCombine"

    it "rejects a composition of unrelated types" $ do
      let expected =
            InvalidCombine
              (combine (concatString " x") (add 1))
              ValueNumber
              ValueString
      case buildTransformationAST (combine (concatString " x") (add 1)) of
        Left err -> err `shouldBe` expected
        Right _ -> expectationFailure "expected an InvalidCombine"

    it "reports the failing composition in DSL syntax" $ do
      case buildTransformationAST (combine (add 1) (equal (Number 3))) of
        Left (InvalidCombine t _ _) -> show t `shouldBe` ("+1 . == 3" :: String)
        Right _ -> expectationFailure "expected an InvalidCombine"
