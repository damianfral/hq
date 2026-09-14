{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.ParserSpec (spec) where

import HQ.Optic
import HQ.Parser (parseQuery)
import HQ.Query (Query (..))
import Relude hiding (Compose, id)
import Test.Syd

spec :: Spec
spec = describe "HQ.Parser" $ do
  parserSpec
  prismParserSpec

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

prismParserSpec :: Spec
prismParserSpec = describe "parseQuery (prisms)" $ do
  it "parses _String" $ do
    parseQuery "view _String" `shouldBe` Right (Preview _String)

  it "parses _Number" $ do
    parseQuery "view _Number" `shouldBe` Right (Preview _Number)

  it "parses _Bool" $ do
    parseQuery "view _Bool" `shouldBe` Right (Preview _Bool)

  it "parses _Null" $ do
    parseQuery "view _Null" `shouldBe` Right (Preview _Null)

  it "parses _Array" $ do
    parseQuery "view _Array" `shouldBe` Right (Preview _Array)

  it "parses _Object" $ do
    parseQuery "view _Object" `shouldBe` Right (Preview _Object)

  it "parses _Just" $ do
    parseQuery "view _Just" `shouldBe` Right (Preview _Just)

  it "parses _1" $ do
    parseQuery "view _1" `shouldBe` Right (Preview _1)

  it "parses _2" $ do
    parseQuery "view _2" `shouldBe` Right (Preview _2)

  it "parses prism in composition with each" $ do
    parseQuery "fold each._String" `shouldBe` Right (Fold (compose each _String))

  it "parses prism in composition with field" $ do
    parseQuery "view #data._Number"
      `shouldBe` Right (Preview (compose (field "data") _Number))

  it "parses prism composed with prism" $ do
    parseQuery "fold _Array._1" `shouldBe` Right (Fold (compose _Array _1))
