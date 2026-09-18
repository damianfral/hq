{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Query.ParserSpec (spec) where

import Data.Aeson (Value (..))
import HQ.Optic
import HQ.Query (Query (..))
import HQ.Query.Parser (parseQuery)
import HQ.Transformation (add, combine, concatString, constValue, equal, trim)
import Relude hiding (Compose, id)
import Test.Syd

spec :: Spec
spec = describe "HQ.Query.Parser" $ do
  parserSpec
  prismParserSpec
  overParserSpec
  setParserSpec

parserSpec :: Spec
parserSpec = describe "parseQuery" $ do
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
      parseQuery "fold each" `shouldBe` Right (Fold each)

    it "parses each in composition" $ do
      let expected = Preview $ compose (field "foo") each
      parseQuery "preview #foo.each" `shouldBe` Right expected

    it "parses each at the start of composition" $ do
      let expected = Preview $ compose each (field "foo")
      parseQuery "preview each.#foo" `shouldBe` Right expected

  describe "field optic" $ do
    it "parses a simple field" $ do
      parseQuery "preview #name" `shouldBe` Right (Preview (field "name"))

    it "parses a field with underscores" $ do
      parseQuery "fold #my_field" `shouldBe` Right (Fold (field "my_field"))

    it "parses a field with numbers" $ do
      parseQuery "preview #field123" `shouldBe` Right (Preview (field "field123"))

    it "parses a field with mixed alphanumeric and underscores" $ do
      parseQuery "preview #foo_bar_1" `shouldBe` Right (Preview (field "foo_bar_1"))

  describe "id optic" $ do
    it "parses id standalone" $ do
      parseQuery "fold id" `shouldBe` Right (Fold id)

    it "parses id in composition" $ do
      let composed = Preview $ compose id (field "foo")
      parseQuery "preview id.#foo" `shouldBe` Right composed

  describe "whitespace handling" $ do
    it "handles extra whitespace around the query" $ do
      parseQuery "  preview #foo  " `shouldBe` Right (Preview (field "foo"))

    it "handles whitespace around the dot separator" $ do
      let expected = Preview $ compose (field "foo") (field "bar")
      parseQuery "preview #foo . #bar" `shouldBe` Right expected

    it "handles no whitespace" $ do
      parseQuery "preview#foo" `shouldBe` Right (Preview (field "foo"))

    it "handles tabs" $ do
      parseQuery "\tpreview\t#foo\t" `shouldBe` Right (Preview (field "foo"))

  describe "error cases" $ do
    it "rejects empty input" $ case parseQuery "" of
      Left _ -> pure ()
      Right q -> expectationFailure $ "Expected parse error, got: " <> show q

    it "rejects unknown operation" $ case parseQuery "unknown #foo" of
      Left _ -> pure ()
      Right q -> expectationFailure $ "Expected parse error, got: " <> show q

    it "rejects missing optic after operation" $ case parseQuery "fold" of
      Left _ -> pure ()
      Right q -> expectationFailure $ "Expected parse error, got: " <> show q

    it "rejects field without hash" $ case parseQuery "fold foo" of
      Left _ -> pure ()
      Right q -> expectationFailure $ "Expected parse error, got: " <> show q

    it "rejects hash without identifier" $ case parseQuery "fold #" of
      Left _ -> pure ()
      Right q -> expectationFailure $ "Expected parse error, got: " <> show q

prismParserSpec :: Spec
prismParserSpec = describe "parseQuery (prisms)" $ do
  it "parses _String" $ do
    parseQuery "fold _String" `shouldBe` Right (Fold _String)

  it "parses _Number" $ do
    parseQuery "fold _Number" `shouldBe` Right (Fold _Number)

  it "parses _Bool" $ do
    parseQuery "fold _Bool" `shouldBe` Right (Fold _Bool)

  it "parses _Null" $ do
    parseQuery "fold _Null" `shouldBe` Right (Fold _Null)

  it "parses _Array" $ do
    parseQuery "fold _Array" `shouldBe` Right (Fold _Array)

  it "parses _Object" $ do
    parseQuery "fold _Object" `shouldBe` Right (Fold _Object)

  it "parses _Just" $ parseQuery "fold _Just" `shouldBe` Right (Fold _Just)

  it "parses _1" $ parseQuery "fold _1" `shouldBe` Right (Fold _1)

  it "parses _2" $ parseQuery "fold _2" `shouldBe` Right (Fold _2)

  it "parses prism in composition with each" $ do
    parseQuery "fold each._String"
      `shouldBe` Right (Fold (compose each _String))

  it "parses prism in composition with field" $ do
    parseQuery "fold #data._Number"
      `shouldBe` Right (Fold (compose (field "data") _Number))

  it "parses prism composed with prism" $ do
    parseQuery "fold _Array._1" `shouldBe` Right (Fold (compose _Array _1))

overParserSpec :: Spec
overParserSpec = describe "parseQuery (over)" $ do
  it "parses over with a field" $ do
    parseQuery "over #foo +1" `shouldBe` Right (Over (field "foo") (add 1))

  it "parses over with each" $ do
    parseQuery "over each trim" `shouldBe` Right (Over each trim)

  it "parses over with composed optics" $ do
    parseQuery "over #foo.each +1"
      `shouldBe` Right (Over (compose (field "foo") each) (add 1))

  it "parses over with a composed transformation" $ do
    parseQuery "over #n +1 . == 3"
      `shouldBe` Right (Over (field "n") (combine (add 1) (equal (Number 3))))

  it "parses over with a string concatenation" $ do
    parseQuery "over #title ++\"!\""
      `shouldBe` Right (Over (field "title") (concatString "!"))

  it "parses over with no whitespace" $ do
    parseQuery "over#foo+1" `shouldBe` Right (Over (field "foo") (add 1))

  it "rejects over without a transformation" $ case parseQuery "over #foo" of
    Left _ -> pure ()
    Right q -> expectationFailure $ "Expected parse error, got: " <> show q

setParserSpec :: Spec
setParserSpec = describe "parseQuery (set)" $ do
  it "parses set as a constant over" $ do
    parseQuery "set #name \"bob\""
      `shouldBe` Right (Over (field "name") (constValue (String "bob")))

  it "parses set with a number value" $ do
    parseQuery "set each 0" `shouldBe` Right (Over each (constValue (Number 0)))

  it "parses set with a composed optic" $ do
    parseQuery "set #users.each.#name \"anon\""
      `shouldBe` Right
        ( Over
            (compose (compose (field "users") each) (field "name"))
            (constValue (String "anon"))
        )
