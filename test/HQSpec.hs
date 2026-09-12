{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQSpec (spec) where

import Control.Comonad.Cofree (Cofree ((:<)))
import qualified Data.Map.Lazy as Map
import qualified Data.Vector as V
import HQ
import Relude hiding (Compose)
import Test.Syd

mkAST :: Cardinality -> OpticF (Cofree OpticF Cardinality) -> AST
mkAST c f = AST (c :< f)

spec :: Spec
spec = describe "HQ" $ do
  parserSpec
  typecheckerSpec
  runnerSpec

parserSpec :: Spec
parserSpec = describe "parseQuery" $ do
  describe "view operation" $ do
    it "parses view with a field" $ do
      parseQuery "view #foo" `shouldBe` Right (Preview (field "foo"))

    it "parses view with each" $ do
      parseQuery "view each" `shouldBe` Right (Preview each)

    it "parses view with composed optics" $ do
      let expected = Preview $ compose (field "foo") (field "bar")
      parseQuery "view #foo.#bar" `shouldBe` Right expected

    it "parses view with deeply composed optics" $ do
      let expected =
            Preview $ compose (compose (field "a") (field "b")) (field "c")
      parseQuery "view #a.#b.#c" `shouldBe` Right expected

  describe "fold operation" $ do
    it "parses fold with a field" $ do
      parseQuery "fold #foo" `shouldBe` Right (Fold (field "foo"))

    it "parses fold with each" $ do
      parseQuery "fold each" `shouldBe` Right (Fold each)

    it "parses fold with composed optics" $ do
      let expected = Fold $ compose (compose (field "foo") each) (field "bar")
      parseQuery "fold #foo.each.#bar" `shouldBe` Right expected

  describe "each optic" $ do
    it "parses each standalone" $ do
      parseQuery "view each" `shouldBe` Right (Preview each)

    it "parses each in composition" $ do
      let expected = Preview $ compose (field "foo") each
      parseQuery "view #foo.each" `shouldBe` Right expected

    it "parses each at the start of composition" $ do
      let expected = Preview $ compose each (field "foo")
      parseQuery "view each.#foo" `shouldBe` Right expected

  describe "field optic" $ do
    it "parses a simple field" $ do
      parseQuery "view #name" `shouldBe` Right (Preview (field "name"))

    it "parses a field with underscores" $ do
      let expected = Preview (field "my_field")
      parseQuery "view #my_field" `shouldBe` Right expected

    it "parses a field with numbers" $ do
      let expected = Preview (field "field123")
      parseQuery "view #field123" `shouldBe` Right expected

    it "parses a field with mixed alphanumeric and underscores" $ do
      let expected = Preview (field "foo_bar_1")
      parseQuery "view #foo_bar_1" `shouldBe` Right expected

  describe "whitespace handling" $ do
    it "handles extra whitespace around the query" $ do
      parseQuery "  view #foo  " `shouldBe` Right (Preview (field "foo"))

    it "handles whitespace around the dot separator" $ do
      let expected = Preview $ compose (field "foo") (field "bar")
      parseQuery "view #foo . #bar" `shouldBe` Right expected

    it "handles no whitespace" $ do
      parseQuery "view#foo" `shouldBe` Right (Preview (field "foo"))

    it "handles tabs" $ do
      parseQuery "\tview\t#foo\t" `shouldBe` Right (Preview (field "foo"))

  describe "error cases" $ do
    it "rejects empty input" $ case parseQuery "" of
      Left _ -> pure ()
      Right q -> expectationFailure $ "Expected parse error, got: " <> show q

    it "rejects unknown operation" $ case parseQuery "unknown #foo" of
      Left _ -> pure ()
      Right q -> expectationFailure $ "Expected parse error, got: " <> show q

    it "rejects missing optic after operation" $ case parseQuery "view" of
      Left _ -> pure ()
      Right q -> expectationFailure $ "Expected parse error, got: " <> show q

    it "rejects field without hash" $ case parseQuery "view foo" of
      Left _ -> pure ()
      Right q -> expectationFailure $ "Expected parse error, got: " <> show q

    it "rejects hash without identifier" $ case parseQuery "view #" of
      Left _ -> pure ()
      Right q -> expectationFailure $ "Expected parse error, got: " <> show q

typecheckerSpec :: Spec
typecheckerSpec = describe "typecheck" $ do
  it "accepts a valid field with One cardinality" $ do
    let ast = mkAST One (Field "name")
    typecheck ast `shouldBe` Right (field "name")

  it "accepts a valid each with Many cardinality" $ do
    let ast = mkAST Many Each
    typecheck ast `shouldBe` Right each

  it "rejects a field with Many cardinality" $ do
    let ast = mkAST Many (Field "name")
    typecheck ast `shouldBe` Left (InvalidCardinality One Many)

  it "rejects an each with One cardinality" $ do
    let ast = mkAST One Each
    typecheck ast `shouldBe` Left (InvalidCardinality Many One)

  it "accepts composed One-One" $ do
    let left = One :< Field "a"
        right = One :< Field "b"
        ast = mkAST One (Compose left right)
    typecheck ast `shouldBe` Right (compose (field "a") (field "b"))

  it "accepts composed One-Many" $ do
    let left = One :< Field "a"
        right = Many :< Each
        ast = mkAST Many (Compose left right)
    typecheck ast `shouldBe` Right (compose (field "a") each)

  it "accepts composed Many-One" $ do
    let left = Many :< Each
        right = One :< Field "b"
        ast = mkAST Many (Compose left right)
    typecheck ast `shouldBe` Right (compose each (field "b"))

  it "accepts composed Many-Many" $ do
    let left = Many :< Each
        right = Many :< Each
        ast = mkAST Many (Compose left right)
    typecheck ast `shouldBe` Right (compose each each)

  it "rejects composed with wrong annotation" $ do
    let left = One :< Field "a"
        right = One :< Field "b"
        ast = mkAST Many (Compose left right)
    typecheck ast `shouldBe` Left (InvalidCardinality One Many)

runnerSpec :: Spec
runnerSpec = describe "run" $ do
  it "extracts a field from an object" $ do
    let optic = field "name"
        value = Object (Map.fromList [("name", String "alice")])
    runTraversal optic value `shouldBe` [String "alice"]

  it "returns empty list when field is missing" $ do
    let optic = field "name"
        value = Object (Map.fromList [("age", Number 30)])
    runTraversal optic value `shouldBe` []

  it "returns empty list for non-object value" $ do
    runTraversal (field "name") Null `shouldBe` []
    runTraversal (field "name") (Bool True) `shouldBe` []
    runTraversal (field "name") (Number 42) `shouldBe` []
    runTraversal (field "name") (String "hi") `shouldBe` []
    runTraversal (field "name") (Array V.empty) `shouldBe` []

  it "iterates over array elements with each" $ do
    let values = V.fromList [Number 1, Number 2, Number 3]
    runTraversal each (Array values) `shouldBe` [Number 1, Number 2, Number 3]

  it "returns empty list for each on non-array" $ do
    runTraversal each Null `shouldBe` []
    runTraversal each (Object Map.empty) `shouldBe` []
    runTraversal each (String "hello") `shouldBe` []

  it "composes field then field" $ do
    let optic = compose (field "a") (field "b")
        aValue = Object $ fromList [("b", Number 42)]
        value = Object $ fromList [("a", aValue)]
    runTraversal optic value `shouldBe` [Number 42]

  it "composes field then each" $ do
    let optic = compose (field "items") each
        items = V.fromList [Number 1, Number 2]
        value = Object $ fromList [("items", Array items)]
    runTraversal optic value `shouldBe` [Number 1, Number 2]

  it "composes each then field" $ do
    let optic = compose each (field "name")
        people =
          V.fromList
            [ Object $ fromList [("name", String "alice")],
              Object $ fromList [("name", String "bob")]
            ]
    runTraversal optic (Array people) `shouldBe` [String "alice", String "bob"]

  it "composes each then each" $ do
    let optic = compose each each
        nested =
          V.fromList
            [ Array (V.fromList [Number 1, Number 2]),
              Array (V.fromList [Number 3])
            ]
        value = Array nested
    runTraversal optic value `shouldBe` [Number 1, Number 2, Number 3]

  it "composes three levels deep" $ do
    let optic = compose (compose (field "a") (field "b")) (field "c")
        bValue = Object (fromList [("c", String "deep")])
        value = Object (fromList [("a", Object (fromList [("b", bValue)]))])
    runTraversal optic value `shouldBe` [String "deep"]

  it "composes field, each, field" $ do
    let optic = compose (compose (field "users") each) (field "name")
        users =
          V.fromList
            [ Object $ fromList [("name", String "alice"), ("age", Number 30)],
              Object $ fromList [("name", String "bob"), ("age", Number 25)]
            ]
        value = Object $ fromList [("users", Array users)]
    runTraversal optic value `shouldBe` [String "alice", String "bob"]
