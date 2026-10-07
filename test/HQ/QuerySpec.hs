{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.QuerySpec (spec) where

import Data.Aeson (Value (..))
import HQ.Optic
import HQ.Query
import HQ.Transformation hiding (Compose)
import qualified HQ.Transformation as T
import HQ.Transformation.AST (TransformationTypeError (..))
import HQ.Transformation.TransformationType (ValueType (..))
import Relude hiding (Compose, filter, id, or)
import Test.Syd

spec :: Spec
spec = describe "HQ.Query" $ do
  describe "typecheckQuery" $ do
    it "accepts fold with a field" $ do
      let q = Fold (Field "name")
      typecheckQuery q `shouldBe` Right q

    it "accepts preview with a field" $ do
      let q = Preview (Field "name")
      typecheckQuery q `shouldBe` Right q

    it "accepts preview with a prism" $ do
      let q = Preview $ Prism PString
      typecheckQuery q `shouldBe` Right q

    it "accepts preview with id" $ do
      let q = Preview Id
      typecheckQuery q `shouldBe` Right q

    it "accepts preview with a traversal" $ do
      let q = Preview Each
      typecheckQuery q `shouldBe` Right q

    it "accepts preview with a traversal composition" $ do
      let q = Preview (Compose Each (Field "x"))
      typecheckQuery q `shouldBe` Right q

    it "accepts preview with a filtered traversal" $ do
      let q = Preview (Compose Each (Filter (Field "age") (Equal (Number 29))))
      typecheckQuery q `shouldBe` Right q

    it "rejects over with a mismatched focus type" $ do
      let optic = Compose Each (Prism PNumber)
          t = ConcatString "!"
      case typecheckQuery (Over optic t) of
        Left (InvalidFocusType _ _ focus expected) -> do
          focus `shouldBe` ValueNumber
          expected `shouldBe` ValueString
        Left err ->
          expectationFailure $ "wrong error: " <> show err
        Right _ -> expectationFailure "expected a type error"

    it "accepts over with a matching focus type" $ do
      let q = Over (Compose Each (Prism PNumber)) (Add 1)
      typecheckQuery q `shouldBe` Right q

    it "rejects a filter predicate with a mismatched input type" $ do
      let bad = Filter (Prism PNumber) (IsPrefixOf "a")
      case typecheckQuery (Fold bad) of
        Left (InvalidFocusType _ _ focus expected) -> do
          focus `shouldBe` ValueNumber
          expected `shouldBe` ValueString
        Left err ->
          expectationFailure $ "wrong error: " <> show err
        Right _ -> expectationFailure "expected a type error"

    it "accepts over with a field" $ do
      let q = Over (Field "age") (Add 1)
      typecheckQuery q `shouldBe` Right q

    it "accepts delete with an index" $ do
      let q = Delete (Ix 0)
      typecheckQuery q `shouldBe` Right q

    it "accepts fold with keys" $ do
      let q = Fold Keys
      typecheckQuery q `shouldBe` Right q

    it "rejects over with a mismatched composition" $ do
      let bad = T.Compose (Add 1) (Equal (Number 3))
      case typecheckQuery (Over Each bad) of
        Left (InvalidTransformationType err) ->
          err `shouldBe` InvalidCompose bad ValueBool ValueNumber
        Left err ->
          expectationFailure $ "wrong error: " <> show err
        Right _ -> expectationFailure "expected a type error"

    it "rejects over with a non-boolean or branch" $ do
      let bad = Or (Add 1) (Equal (Number 2))
      case typecheckQuery (Over Each bad) of
        Left (InvalidTransformationType err) ->
          err `shouldBe` InvalidOr bad ValueNumber
        Left err ->
          expectationFailure $ "wrong error: " <> show err
        Right _ -> expectationFailure "expected a type error"

    it "accepts equal after a step of any type" $ do
      let q = Over Each (T.Compose (Equal (Number 1)) Trim)
      typecheckQuery q `shouldBe` Right q

    it "accepts fold with a filter" $ do
      let q = Fold (Filter (Field "a") (Equal (Number 1)))
      typecheckQuery q `shouldBe` Right q

    it "accepts preview with a filter" $ do
      let q = Preview (Filter (Field "a") (Equal (Number 1)))
      typecheckQuery q `shouldBe` Right q

    it "rejects a filter with a non-boolean predicate" $ do
      let bad = Filter (Field "a") (Add 1)
      case typecheckQuery (Fold bad) of
        Left (InvalidTransformationType err) ->
          err `shouldBe` InvalidFilter (Add 1) ValueNumber
        Left err ->
          expectationFailure $ "wrong error: " <> show err
        Right _ -> expectationFailure "expected a type error"

    it "rejects a filter with an ill-formed predicate" $ do
      let bad = T.Compose (Add 1) (Equal (Number 3))
      case typecheckQuery (Fold (Filter (Field "a") bad)) of
        Left (InvalidTransformationType err) ->
          err `shouldBe` InvalidCompose bad ValueBool ValueNumber
        Left err ->
          expectationFailure $ "wrong error: " <> show err
        Right _ -> expectationFailure "expected a type error"

    it "rejects a nested filter with a bad predicate" $ do
      let inner = Filter (Field "b") (Add 2)
          bad = Filter (Compose Each inner) (Equal (Number 1))
      case typecheckQuery (Fold bad) of
        Left (InvalidTransformationType err) ->
          err `shouldBe` InvalidFilter (Add 2) ValueNumber
        Left err ->
          expectationFailure $ "wrong error: " <> show err
        Right _ -> expectationFailure "expected a type error"
