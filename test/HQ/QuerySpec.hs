{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.QuerySpec (spec) where

import qualified Data.Map.Lazy as Map
import qualified Data.Vector as V
import HQ
import HQ.Optic
import HQ.Optic.OpticType (OpticType (..))
import HQ.Query
import HQ.Value
import Relude hiding (Compose, id)
import Test.Syd

spec :: Spec
spec = describe "HQ.Query" $ do
  typecheckerSpec
  prismTypecheckerSpec
  executeQuerySpec

typecheckerSpec :: Spec
typecheckerSpec = describe "typecheckQuery" $ do
  -- Preview expects OpticPrism
  it "accepts Preview with a prism" $ do
    typecheckQuery (Preview _String) `shouldBe` Right (Preview _String)

  it "rejects Preview with a field (Lens)" $ do
    typecheckQuery (Preview (field "name"))
      `shouldBe` Left (InvalidOpticType OpticPrism OpticLens)

  it "rejects Preview with each (Traversal)" $ do
    typecheckQuery (Preview each)
      `shouldBe` Left (InvalidOpticType OpticPrism OpticTraversal)

  it "rejects Preview with id (Lens)" $ do
    typecheckQuery (Preview id)
      `shouldBe` Left (InvalidOpticType OpticPrism OpticLens)

  -- Fold expects OpticTraversal
  it "accepts Fold with each" $ do
    typecheckQuery (Fold each) `shouldBe` Right (Fold each)

  it "rejects Fold with a field (Lens)" $ do
    typecheckQuery (Fold (field "name"))
      `shouldBe` Left (InvalidOpticType OpticTraversal OpticLens)

  it "rejects Fold with id (Lens)" $ do
    typecheckQuery (Fold id)
      `shouldBe` Left (InvalidOpticType OpticTraversal OpticLens)

  -- Set expects OpticTraversal
  it "accepts Set with each" $ do
    typecheckQuery (Set each (Number 1)) `shouldBe` Right (Set each (Number 1))

  it "rejects Set with a field (Lens)" $ do
    typecheckQuery (Set (field "name") (String "x"))
      `shouldBe` Left (InvalidOpticType OpticTraversal OpticLens)

  -- Delete expects OpticTraversal
  it "accepts Delete with each" $ do
    typecheckQuery (Delete each) `shouldBe` Right (Delete each)

  it "rejects Delete with a field (Lens)" $ do
    typecheckQuery (Delete (field "name"))
      `shouldBe` Left (InvalidOpticType OpticTraversal OpticLens)

  -- Composed optics: composition type determines accept/reject
  it "accepts Fold with composed each.field (Traversal)" $ do
    typecheckQuery (Fold (compose each (field "name")))
      `shouldBe` Right (Fold (compose each (field "name")))

  it "accepts Fold with composed each._String (Traversal)" $ do
    typecheckQuery (Fold (compose each _String))
      `shouldBe` Right (Fold (compose each _String))

  it "rejects Preview with composed each.field (Traversal)" $ do
    typecheckQuery (Preview (compose each (field "name")))
      `shouldBe` Left (InvalidOpticType OpticPrism OpticTraversal)

  it "accepts Preview with composed _String._String (Prism)" $ do
    typecheckQuery (Preview (compose _String _String))
      `shouldBe` Right (Preview (compose _String _String))

  it "rejects Fold with composed _String._String (Prism)" $ do
    typecheckQuery (Fold (compose _String _String))
      `shouldBe` Left (InvalidOpticType OpticTraversal OpticPrism)

  it "accepts Fold with composed id.each (Traversal)" $ do
    typecheckQuery (Fold (compose id each))
      `shouldBe` Right (Fold (compose id each))

  it "rejects Fold with composed id.field (Lens)" $ do
    typecheckQuery (Fold (compose id (field "name")))
      `shouldBe` Left (InvalidOpticType OpticTraversal OpticLens)

  it "accepts Fold with composed each.each (Traversal)" $ do
    typecheckQuery (Fold (compose each each))
      `shouldBe` Right (Fold (compose each each))

prismTypecheckerSpec :: Spec
prismTypecheckerSpec = describe "typecheckQuery (prisms)" $ do
  it "accepts Preview _String (Prism matches Prism)" $ do
    typecheckQuery (Preview _String) `shouldBe` Right (Preview _String)

  it "accepts Preview _Number" $ do
    typecheckQuery (Preview _Number) `shouldBe` Right (Preview _Number)

  it "accepts Preview _Bool" $ do
    typecheckQuery (Preview _Bool) `shouldBe` Right (Preview _Bool)

  it "accepts Preview _Null" $ do
    typecheckQuery (Preview _Null) `shouldBe` Right (Preview _Null)

  it "accepts Preview _Array" $ do
    typecheckQuery (Preview _Array) `shouldBe` Right (Preview _Array)

  it "accepts Preview _Object" $ do
    typecheckQuery (Preview _Object) `shouldBe` Right (Preview _Object)

  it "accepts Preview _Just" $ do
    typecheckQuery (Preview _Just) `shouldBe` Right (Preview _Just)

  it "accepts Preview _1" $ do
    typecheckQuery (Preview _1) `shouldBe` Right (Preview _1)

  it "accepts Preview _2" $ do
    typecheckQuery (Preview _2) `shouldBe` Right (Preview _2)

  it "rejects Fold _String (Prism != Traversal)" $ do
    typecheckQuery (Fold _String)
      `shouldBe` Left (InvalidOpticType OpticTraversal OpticPrism)

  it "rejects Fold _Number" $ do
    typecheckQuery (Fold _Number)
      `shouldBe` Left (InvalidOpticType OpticTraversal OpticPrism)

  it "rejects Fold _Just" $ do
    typecheckQuery (Fold _Just)
      `shouldBe` Left (InvalidOpticType OpticTraversal OpticPrism)

  it "rejects Fold _1" $ do
    typecheckQuery (Fold _1)
      `shouldBe` Left (InvalidOpticType OpticTraversal OpticPrism)

  it "accepts Fold each._String (Traversal)" $ do
    typecheckQuery (Fold (compose each _String))
      `shouldBe` Right (Fold (compose each _String))

  it "rejects Preview each._String (Traversal != Prism)" $ do
    typecheckQuery (Preview (compose each _String))
      `shouldBe` Left (InvalidOpticType OpticPrism OpticTraversal)

executeQuerySpec :: Spec
executeQuerySpec = describe "executeQuery" $ do
  it "preview returns first match" $ do
    let v = Array $ V.fromList [Number 1, Number 2]
    executeQuery (Preview each) v `shouldBe` Single (Just (Number 1))

  it "preview returns Nothing when no match" $ do
    let v = Object Map.empty
    executeQuery (Preview (field "missing")) v `shouldBe` Single Nothing

  it "fold returns all matches" $ do
    let v = Array $ V.fromList [Number 1, Number 2, Number 3]
    executeQuery (Fold each) v `shouldBe` Multi [Number 1, Number 2, Number 3]

  it "set replaces all focused values" $ do
    let v = Object $ fromList [("name", String "alice")]
    let expected = Single (Just (Object $ fromList [("name", String "bob")]))
    executeQuery (Set (field "name") (String "bob")) v `shouldBe` expected

  it "delete removes focused values" $ do
    let v = Object $ fromList [("name", String "alice"), ("age", Number 30)]
    let expected = Single (Just (Object $ fromList [("age", Number 30)]))
    executeQuery (Delete (field "name")) v `shouldBe` expected
