{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQSpec (spec) where

import HQ
import Relude hiding (Compose)
import Test.Syd

spec :: Spec
spec = describe "HQ" $ do
  describe "parseQuery" $ do
    describe "view operation" $ do
      it "parses view with a field" $ do
        parseQuery "view #foo" `shouldBe` Right (Preview (field "foo"))

      it "parses ^. with a field" $ do
        parseQuery "^. each" `shouldBe` Right (Preview each)

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

      it "parses ^.. with a field" $ do
        parseQuery "^.. #foo" `shouldBe` Right (Fold (field "foo"))

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
