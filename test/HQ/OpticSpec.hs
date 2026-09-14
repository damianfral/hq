{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.OpticSpec (spec) where

import Data.Fix (Fix (..))
import qualified Data.Map.Lazy as Map
import qualified Data.Vector as V
import HQ
import HQ.Optic
import HQ.Value (Value (..))
import Relude hiding (Compose, id)
import Test.Syd

spec :: Spec
spec = describe "HQ.Optic" $ do
  runnerSpec
  prismFoldSpec
  prismOverSpec
  prismDeleteSpec
  prismCompositionSpec

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

prismFoldSpec :: Spec
prismFoldSpec = describe "runFold (prisms)" $ do
  describe "_String" $ do
    it "matches a String value" $ do
      runFold _String (String "hello") `shouldBe` [String "hello"]

    it "rejects a Number value" $ do
      runFold _String (Number 42) `shouldBe` []

    it "rejects null" $ do
      runFold _String Null `shouldBe` []

  describe "_Number" $ do
    it "matches a Number value" $ do
      runFold _Number (Number 42) `shouldBe` [Number 42]

    it "rejects a String value" $ do
      runFold _Number (String "hello") `shouldBe` []

  describe "_Bool" $ do
    it "matches a Bool value" $ do
      runFold _Bool (Bool True) `shouldBe` [Bool True]

    it "rejects a Number value" $ do
      runFold _Bool (Number 1) `shouldBe` []

  describe "_Null" $ do
    it "matches Null" $ do
      runFold _Null Null `shouldBe` [Null]

    it "rejects non-null" $ do
      runFold _Null (Number 1) `shouldBe` []

  describe "_Array" $ do
    it "matches an Array" $ do
      runFold _Array (Array (V.fromList [Number 1]))
        `shouldBe` [Array (V.fromList [Number 1])]

    it "rejects a non-array" $ do
      runFold _Array (Number 1)
        `shouldBe` []

  describe "_Object" $ do
    it "matches an Object" $ do
      runFold _Object (Object (fromList [("a", Number 1)]))
        `shouldBe` [Object (fromList [("a", Number 1)])]

    it "rejects a non-object" $ do
      runFold _Object (Number 1) `shouldBe` []

  describe "_Just" $ do
    it "matches a non-null value" $ do
      runFold _Just (Number 42) `shouldBe` [Number 42]

    it "rejects Null" $ do
      runFold _Just Null `shouldBe` []

  describe "_1" $ do
    it "gets the first element of an array" $ do
      runFold _1 (Array (V.fromList [Number 1, Number 2, Number 3]))
        `shouldBe` [Number 1]

    it "returns empty for single-element array" $ do
      runFold _1 (Array (V.fromList [Number 1])) `shouldBe` [Number 1]

    it "returns empty for empty array" $ do
      runFold _1 (Array V.empty) `shouldBe` []

    it "returns empty for non-array" $ do
      runFold _1 (Number 42) `shouldBe` []

  describe "_2" $ do
    it "gets the second element of an array" $ do
      runFold _2 (Array (V.fromList [Number 1, Number 2, Number 3]))
        `shouldBe` [Number 2]

    it "returns empty for single-element array" $ do
      runFold _2 (Array (V.fromList [Number 1])) `shouldBe` []

    it "returns empty for empty array" $ do
      runFold _2 (Array V.empty) `shouldBe` []

    it "returns empty for non-array" $ do
      runFold _2 (Number 42) `shouldBe` []

prismOverSpec :: Spec
prismOverSpec = describe "runOver (prisms)" $ do
  it "modifies a String via _String" $ do
    runOver (Fix PrismString) (const (String "changed")) (String "hello")
      `shouldBe` String "changed"

  it "does not modify non-matching value via _String" $ do
    runOver (Fix PrismString) (const (String "changed")) (Number 42)
      `shouldBe` Number 42

  it "modifies a Number via _Number" $ do
    runOver (Fix PrismNumber) (const (Number 99)) (Number 42)
      `shouldBe` Number 99

  it "modifies Null via _Null" $ do
    runOver (Fix PrismNull) (const (String "gone")) Null
      `shouldBe` String "gone"

  it "modifies a non-null value via _Just" $ do
    runOver (Fix PrismJust) (const (Number 99)) (String "hello")
      `shouldBe` Number 99

  it "does not modify Null via _Just" $ do
    runOver (Fix PrismJust) (const (Number 99)) Null `shouldBe` Null

  it "modifies first element via _1" $ do
    let v = Array (V.fromList [Number 1, Number 2])
    runOver (Fix Prism1) (const (Number 99)) v
      `shouldBe` Array (V.fromList [Number 99, Number 2])

  it "modifies second element via _2" $ do
    let v = Array (V.fromList [Number 1, Number 2, Number 3])
    runOver (Fix Prism2) (const (Number 99)) v
      `shouldBe` Array (V.fromList [Number 1, Number 99, Number 3])

  it "_1 does nothing on non-array" $ do
    runOver (Fix Prism1) (const (Number 99)) (Number 42)
      `shouldBe` Number 42

  it "_2 does nothing on short array" $ do
    let v = Array (V.fromList [Number 1])
    runOver (Fix Prism2) (const (Number 99)) v
      `shouldBe` Array (V.fromList [Number 1])

prismDeleteSpec :: Spec
prismDeleteSpec = describe "runDelete (prisms)" $ do
  it "deletes a String via _String" $ do
    runDelete _String (String "hello") `shouldBe` Null

  it "does not delete non-matching via _String" $ do
    runDelete _String (Number 42) `shouldBe` Number 42

  it "deletes a Number via _Number" $ do
    runDelete _Number (Number 42) `shouldBe` Null

  it "deletes Null via _Null" $ do
    runDelete _Null Null `shouldBe` Null

  it "does not delete non-null via _Just" $ do
    runDelete _Just (Number 42) `shouldBe` Number 42

  it "does not delete Null via _Just" $ do
    runDelete _Just Null `shouldBe` Null

  it "replaces first element with Null via _1" $ do
    runDelete _1 (Array (V.fromList [Number 1, Number 2]))
      `shouldBe` Array (V.fromList [Null, Number 2])

  it "replaces second element with Null via _2" $ do
    runDelete _2 (Array (V.fromList [Number 1, Number 2, Number 3]))
      `shouldBe` Array (V.fromList [Number 1, Null, Number 3])

  it "_1 does nothing on non-array" $ do
    runDelete _1 (Number 42) `shouldBe` Number 42

  it "_2 does nothing on short array" $ do
    let v = Array (V.fromList [Number 1])
    runDelete _2 v `shouldBe` Array (V.fromList [Number 1])

prismCompositionSpec :: Spec
prismCompositionSpec = describe "prism composition" $ do
  it "each._String filters strings from array" $ do
    let v = Array (V.fromList [Number 1, String "a", Number 2, String "b"])
    runFold (compose each _String) v `shouldBe` [String "a", String "b"]

  it "each._Number filters numbers from array" $ do
    let v = Array (V.fromList [Number 1, String "a", Number 2])
    runFold (compose each _Number) v `shouldBe` [Number 1, Number 2]

  it "field._Just filters null fields" $ do
    let v = Object (fromList [("x", Number 1)])
    runFold (compose (field "x") _Just) v `shouldBe` [Number 1]

  it "field._Just skips null fields" $ do
    let v = Object (fromList [("x", Null)])
    runFold (compose (field "x") _Just) v `shouldBe` []

  it "_Array._1 gets first element after confirming array type" $ do
    let v = Array (V.fromList [Number 42, Number 43])
    runFold (compose _Array _1) v `shouldBe` [Number 42]

  it "each._1 gets first element of each sub-array" $ do
    let v =
          Array
            $ V.fromList
              [ Array (V.fromList [Number 1, Number 2]),
                Array (V.fromList [Number 3, Number 4])
              ]
    runFold (compose each _1) v `shouldBe` [Number 1, Number 3]
