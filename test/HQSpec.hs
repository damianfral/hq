{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQSpec (spec) where

import Control.Comonad.Cofree (Cofree ((:<)))
import Data.Fix (Fix (..))
import qualified Data.Map.Lazy as Map
import qualified Data.Vector as V
import HQ
import HQ.AST
import HQ.Optic
import HQ.Parser (parseQuery)
import HQ.Query (Query (..), Value (..))
import Relude hiding (Compose, id)
import Test.Syd

mkAST :: OpticType -> OpticF (Cofree OpticF OpticType) -> OpticAST
mkAST c f = OpticAST (c :< f)

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

    it "parses view with id" $ do
      parseQuery "view id" `shouldBe` Right (Preview id)

    it "parses view with composed optics" $ do
      parseQuery "view #foo.#bar"
        `shouldBe` Right (Preview $ compose (field "foo") (field "bar"))

    it "parses view with deeply composed optics" $ do
      let expected =
            Preview (compose (compose (field "a") (field "b")) (field "c"))
      parseQuery "view #a.#b.#c" `shouldBe` Right expected

  describe "fold operation" $ do
    it "parses fold with a field" $ do
      parseQuery "fold #foo" `shouldBe` Right (Fold (field "foo"))

    it "parses fold with each" $ do
      parseQuery "fold each" `shouldBe` Right (Fold each)

    it "parses fold with composed optics" $ do
      let expected = Fold (compose (compose (field "foo") each) (field "bar"))
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
      parseQuery "view #my_field" `shouldBe` Right (Preview (field "my_field"))

    it "parses a field with numbers" $ do
      parseQuery "view #field123" `shouldBe` Right (Preview (field "field123"))

    it "parses a field with mixed alphanumeric and underscores" $ do
      parseQuery "view #foo_bar_1" `shouldBe` Right (Preview (field "foo_bar_1"))

  describe "id optic" $ do
    it "parses id standalone" $ do
      parseQuery "view id" `shouldBe` Right (Preview id)

    it "parses id in composition" $ do
      let composed = Preview $ compose id (field "foo")
      parseQuery "view id.#foo" `shouldBe` Right composed

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
  it "accepts a valid field with AffineTraversal type" $ do
    let ast = mkAST OpticAffineTraversal (Field "name")
    typecheck ast `shouldBe` Right (field "name")

  it "accepts a valid each with Traversal type" $ do
    typecheck (mkAST OpticTraversal Each) `shouldBe` Right each

  it "accepts a valid id with Lens type" $ do
    typecheck (mkAST OpticLens Id) `shouldBe` Right id

  it "rejects a field with Lens type" $ do
    typecheck (mkAST OpticAffineTraversal (Field "name"))
      `shouldBe` Right (field "name")

  it "rejects a field with Traversal type" $ do
    let err = InvalidOpticType OpticAffineTraversal OpticTraversal
    typecheck (mkAST OpticTraversal (Field "name")) `shouldBe` Left err

  it "rejects an each with Lens type" $ do
    let err = InvalidOpticType OpticTraversal OpticLens
    typecheck (mkAST OpticLens Each) `shouldBe` Left err

  it "rejects an id with Traversal type" $ do
    let err = InvalidOpticType OpticLens OpticTraversal
    typecheck (mkAST OpticTraversal Id) `shouldBe` Left err

  it "accepts composed Lens-Lens" $ do
    let composed =
          Compose
            (OpticAffineTraversal :< Field "a")
            (OpticAffineTraversal :< Field "b")
    let ast = mkAST OpticAffineTraversal composed
    typecheck ast `shouldBe` Right (compose (field "a") (field "b"))

  it "accepts composed AffineTraversal-Lens" $ do
    let composed =
          Compose
            (OpticAffineTraversal :< Field "a")
            (OpticAffineTraversal :< Field "b")
    let ast = mkAST OpticAffineTraversal composed
    typecheck ast `shouldBe` Right (compose (field "a") (field "b"))

  it "accepts composed Lens-AffineTraversal" $ do
    let composed = Compose (OpticLens :< Id) (OpticAffineTraversal :< Field "b")
    let ast = mkAST OpticAffineTraversal composed
    typecheck ast `shouldBe` Right (compose id (field "b"))

  it "accepts composed Lens-Traversal" $ do
    let composed = Compose (OpticLens :< Id) (OpticTraversal :< Each)
    let ast = mkAST OpticTraversal composed
    typecheck ast `shouldBe` Right (compose id each)

  it "accepts composed Traversal-Lens" $ do
    let composed =
          Compose (OpticTraversal :< Each) (OpticAffineTraversal :< Field "b")
    let ast = mkAST OpticTraversal composed
    typecheck ast `shouldBe` Right (compose each (field "b"))
  --
  -- it "accepts composed AffineTraversal-Prism" $ do
  --   typecheck
  --     (mkAST OpticAffineTraversal
  --       (Compose
  --         (OpticAffineTraversal :< Field "a")
  --         (OpticPrism :< String)))
  --     `shouldBe` Right (compose (field "a") string)

  -- it "accepts composed Prism-Prism" $ do
  --   typecheck
  --     (mkAST OpticPrism
  --       (Compose
  --         (OpticPrism :< String)
  --         (OpticPrism :< String)))
  --     `shouldBe` Right (compose string string)

  -- it "accepts composed Traversal-Prism" $ do
  --   typecheck
  --     (mkAST OpticTraversal
  --       (Compose
  --         (OpticTraversal :< Each)
  --         (OpticPrism :< String)))
  --     `shouldBe` Right (compose each string)

  it "accepts composed Traversal-Traversal" $ do
    let composed = Compose (OpticTraversal :< Each) (OpticTraversal :< Each)
    let ast = mkAST OpticTraversal composed
    typecheck ast `shouldBe` Right (compose each each)

  it "rejects composed with wrong annotation" $ do
    let composed = Compose (OpticAffineTraversal :< Field "a") (OpticLens :< Id)
    let ast = mkAST OpticLens composed
    let err = InvalidOpticType OpticAffineTraversal OpticLens
    typecheck ast `shouldBe` Left err

runnerSpec :: Spec
runnerSpec = describe "run" $ do
  describe "runFold" $ do
    it "extracts a field from an object" $ do
      runFold (field "name") (Object (Map.fromList [("name", String "alice")]))
      `shouldBe` [String "alice"]

    it "returns empty list when field is missing" $ do
      runFold (field "name") (Object (Map.fromList [("age", Number 30)]))
      `shouldBe` []

    it "returns empty list for non-object value" $ do
      runFold (field "name") Null `shouldBe` []
      runFold (field "name") (Bool True) `shouldBe` []
      runFold (field "name") (Number 42) `shouldBe` []
      runFold (field "name") (String "hi") `shouldBe` []
      runFold (field "name") (Array V.empty) `shouldBe` []

    it "iterates over array elements with each" $ do
      let v = Array (V.fromList [Number 1, Number 2, Number 3])
      runFold each v `shouldBe` [Number 1, Number 2, Number 3]

    it "iterates over object values with each" $ do
      let v = Object (Map.fromList [("a", Number 1), ("b", Number 2)])
      runFold each v `shouldBe` [Number 1, Number 2]

    it "returns empty list for each on non-container" $ do
      runFold each Null `shouldBe` []
      runFold each (Number 42) `shouldBe` []
      runFold each (String "hello") `shouldBe` []

    it "returns the value with id" $ do
      let val = Object (Map.fromList [("a", Number 1)])
      runFold id val `shouldBe` [val]

    it "distributes field access over array elements via each" $ do
      let optic = compose each (field "name")
          people =
            V.fromList
              [ Object $ fromList [("name", String "alice"), ("age", Number 30)],
                Object $ fromList [("name", String "bob"), ("age", Number 25)]
              ]
          value = Array people
      runFold optic value `shouldBe` [String "alice", String "bob"]

  describe "runFold (compositions)" $ do
    it "composes field then field" $ do
      let optic = compose (field "a") (field "b")
          aValue = Object $ fromList [("b", Number 42)]
          value = Object $ fromList [("a", aValue)]
      runFold optic value `shouldBe` [Number 42]

    it "composes field then each" $ do
      let optic = compose (field "items") each
          items = V.fromList [Number 1, Number 2]
          value = Object $ fromList [("items", Array items)]
      runFold optic value `shouldBe` [Number 1, Number 2]

    it "composes each then field" $ do
      let optic = compose each (field "name")
          people =
            V.fromList
              [ Object $ fromList [("name", String "alice")],
                Object $ fromList [("name", String "bob")]
              ]
      runFold optic (Array people) `shouldBe` [String "alice", String "bob"]

    it "composes each then each" $ do
      let optic = compose each each
          nested =
            V.fromList
              [ Array (V.fromList [Number 1, Number 2]),
                Array (V.fromList [Number 3])
              ]
          value = Array nested
      runFold optic value `shouldBe` [Number 1, Number 2, Number 3]

    it "composes three levels deep" $ do
      let optic = compose (compose (field "a") (field "b")) (field "c")
          bValue = Object (fromList [("c", String "deep")])
          value = Object (fromList [("a", Object (fromList [("b", bValue)]))])
      runFold optic value `shouldBe` [String "deep"]

    it "composes field, each, field" $ do
      let optic = compose (compose (field "users") each) (field "name")
          users =
            V.fromList
              [ Object $ fromList [("name", String "alice"), ("age", Number 30)],
                Object $ fromList [("name", String "bob"), ("age", Number 25)]
              ]
          value = Object $ fromList [("users", Array users)]
      runFold optic value `shouldBe` [String "alice", String "bob"]

    it "composes id then field" $ do
      let optic = compose id (field "name")
          value = Object $ fromList [("name", String "test")]
      runFold optic value `shouldBe` [String "test"]

  describe "runOver" $ do
    it "modifies a field in an object" $ do
      let value =
            Object $ fromList [("name", String "alice"), ("age", Number 30)]
          result = runOver (Fix $ Field "name") (const (String "bob")) value
          expected =
            Object (fromList [("name", String "bob"), ("age", Number 30)])
      result `shouldBe` expected

    it "does nothing when field is missing" $ do
      let value = Object $ fromList [("age", Number 30)]
          result = runOver (Fix $ Field "name") (const (String "bob")) value
      result `shouldBe` value

    it "modifies each element in an array" $ do
      let value = Array $ V.fromList [Number 1, Number 2, Number 3]
          result = runOver (Fix Each) (const (Number 99)) value
      result `shouldBe` Array (V.fromList [Number 99, Number 99, Number 99])

    it "modifies each value in an object with each" $ do
      let value = Object $ fromList [("a", Number 1), ("b", Number 2)]
          result = runOver (Fix Each) (const (Number 99)) value
      result `shouldBe` Object (fromList [("a", Number 99), ("b", Number 99)])

    it "modifies each element in an array via composed each" $ do
      let optic = Fix (Compose (Fix (Field "items")) (Fix Each))
          x1 = Object $ fromList [("x", Number 1)]
          x2 = Object $ fromList [("x", Number 2)]
          items = Array $ V.fromList [x1, x2]
          value = Object $ fromList [("items", items)]
          result = runOver optic (const (Number 99)) value
          expected =
            Object
              $ fromList [("items", Array $ V.fromList [Number 99, Number 99])]
      result `shouldBe` expected

    it "applies id identity modification" $ do
      let value = Number 42
          result = runOver (Fix Id) (const (String "changed")) value
      result `shouldBe` String "changed"

    it "composes field then each for nested modification" $ do
      let optic = Fix (Compose (Fix (Field "items")) (Fix Each))
          items = Array $ V.fromList [Number 1, Number 2]
          value = Object $ fromList [("items", items)]
          result = runOver optic (const (Number 99)) value
          expected =
            Object
              $ fromList [("items", Array $ V.fromList [Number 99, Number 99])]
      result `shouldBe` expected

  describe "runDelete" $ do
    it "deletes a field from an object" $ do
      let value =
            Object $ fromList [("name", String "alice"), ("age", Number 30)]
      runDelete (field "name") value
        `shouldBe` Object (fromList [("age", Number 30)])

    it "deletes each element from an array" $ do
      runDelete each (Array $ V.fromList [Number 1, Number 2])
        `shouldBe` Array V.empty

    it "deletes each value from an object" $ do
      runDelete each (Object $ fromList [("a", Number 1), ("b", Number 2)])
        `shouldBe` Object Map.empty

    it "replaces id with null" $ do
      runDelete id (Number 42) `shouldBe` Null

    it "composes field then delete each" $ do
      let value =
            Object
              $ fromList
                [("items", Array $ V.fromList [Number 1, Number 2, Number 3])]
      runDelete (compose (field "items") each) value
        `shouldBe` Object (fromList [("items", Array V.empty)])

    it "deletes a field from each element via composed each" $ do
      let optic = compose (field "items") (compose each (field "name"))
          items =
            V.fromList
              [ Object $ fromList [("name", String "alice"), ("age", Number 30)],
                Object $ fromList [("name", String "bob"), ("age", Number 25)]
              ]
          value = Object $ fromList [("items", Array items)]
          expected =
            Object
              $ fromList
                [ ( "items",
                    Array
                      $ V.fromList
                        [ Object $ fromList [("age", Number 30)],
                          Object $ fromList [("age", Number 25)]
                        ]
                  )
                ]
      runDelete optic value `shouldBe` expected

  describe "executeQuery" $ do
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
