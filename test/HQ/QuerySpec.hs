{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.QuerySpec (spec) where

import Data.Aeson (Value (..))
import HQ.Optic
import HQ.Optic.OpticType (OpticType (..))
import HQ.Query
import HQ.Transformation (add, combine, equal, or, trim)
import HQ.Transformation.AST (TransformationTypeError (..))
import HQ.Transformation.TransformationType (ValueType (..))
import Relude hiding (Compose, filter, id, or)
import Test.Syd

spec :: Spec
spec = describe "HQ.Query" $ do
  describe "typecheckQuery" $ do
    it "accepts fold with a field" $ do
      let q = Fold (field "name")
      typecheckQuery q `shouldBe` Right q

    it "accepts preview with a field" $ do
      let q = Preview (field "name")
      typecheckQuery q `shouldBe` Right q

    it "accepts preview with a prism" $ do
      let q = Preview _String
      typecheckQuery q `shouldBe` Right q

    it "accepts preview with id" $ do
      let q = Preview id
      typecheckQuery q `shouldBe` Right q

    it "rejects preview with a traversal" $ do
      case typecheckQuery (Preview each) of
        Left err ->
          err `shouldBe` InvalidOpticType OpticPrism OpticTraversal
        Right _ -> expectationFailure "expected a type error"

    it "rejects preview with a traversal composition" $ do
      case typecheckQuery (Preview (compose each (field "x"))) of
        Left err ->
          err `shouldBe` InvalidOpticType OpticPrism OpticTraversal
        Right _ -> expectationFailure "expected a type error"

    it "accepts over with a field" $ do
      let q = Over (field "age") (add 1)
      typecheckQuery q `shouldBe` Right q

    it "accepts delete with an index" $ do
      let q = Delete (ix 0)
      typecheckQuery q `shouldBe` Right q

    it "accepts fold with keys" $ do
      let q = Fold keys
      typecheckQuery q `shouldBe` Right q

    it "rejects over with a mismatched composition" $ do
      let bad = combine (add 1) (equal (Number 3))
      case typecheckQuery (Over each bad) of
        Left (InvalidTransformationType err) ->
          err `shouldBe` InvalidCombine bad ValueBool ValueNumber
        Left err ->
          expectationFailure $ "wrong error: " <> show err
        Right _ -> expectationFailure "expected a type error"

    it "rejects over with a non-boolean or branch" $ do
      let bad = or (add 1) (equal (Number 2))
      case typecheckQuery (Over each bad) of
        Left (InvalidTransformationType err) ->
          err `shouldBe` InvalidOr bad ValueNumber
        Left err ->
          expectationFailure $ "wrong error: " <> show err
        Right _ -> expectationFailure "expected a type error"

    it "accepts equal after a step of any type" $ do
      let q = Over each (combine (equal (Number 1)) trim)
      typecheckQuery q `shouldBe` Right q

    it "accepts fold with a filter" $ do
      let q = Fold (filter (field "a") (equal (Number 1)))
      typecheckQuery q `shouldBe` Right q

    it "accepts preview with a filter" $ do
      let q = Preview (filter (field "a") (equal (Number 1)))
      typecheckQuery q `shouldBe` Right q

    it "rejects a filter with a non-boolean predicate" $ do
      let bad = filter (field "a") (add 1)
      case typecheckQuery (Fold bad) of
        Left (InvalidTransformationType err) ->
          err `shouldBe` InvalidFilter (add 1) ValueNumber
        Left err ->
          expectationFailure $ "wrong error: " <> show err
        Right _ -> expectationFailure "expected a type error"

    it "rejects a filter with an ill-formed predicate" $ do
      let bad = combine (add 1) (equal (Number 3))
      case typecheckQuery (Fold (filter (field "a") bad)) of
        Left (InvalidTransformationType err) ->
          err `shouldBe` InvalidCombine bad ValueBool ValueNumber
        Left err ->
          expectationFailure $ "wrong error: " <> show err
        Right _ -> expectationFailure "expected a type error"

    it "rejects a nested filter with a bad predicate" $ do
      let inner = filter (field "b") (add 2)
          bad = filter (compose each inner) (equal (Number 1))
      case typecheckQuery (Fold bad) of
        Left (InvalidTransformationType err) ->
          err `shouldBe` InvalidFilter (add 2) ValueNumber
        Left err ->
          expectationFailure $ "wrong error: " <> show err
        Right _ -> expectationFailure "expected a type error"

  describe "renderTypeError" $ do
    it "renders an optic mismatch" $ do
      let err = InvalidOpticType OpticPrism OpticTraversal
      renderTypeError err
        `shouldBe` "optic mismatch: this query needs a prism (at most one target) but the optic is a traversal"

    it "renders a transformation seam mismatch" $ do
      let bad = combine (add 1) (equal (Number 3))
          err = InvalidTransformationType (InvalidCombine bad ValueBool ValueNumber)
      renderTypeError err
        `shouldBe` "transformation mismatch in +1 . == 3: produces boolean but the next step expects number"

    it "renders a non-boolean or branch" $ do
      let bad = or (add 1) (equal (Number 2))
          err = InvalidTransformationType (InvalidOr bad ValueNumber)
      renderTypeError err
        `shouldBe` "transformation mismatch in +1 or == 2: both sides of or must produce booleans, but a branch produces number"

    it "renders a non-boolean filter predicate" $ do
      let err = InvalidTransformationType (InvalidFilter (add 1) ValueNumber)
      renderTypeError err
        `shouldBe` "filter transformation +1 must produce a boolean, but produces number"
