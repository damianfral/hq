{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Query.ParserSpec (spec) where

import Data.Aeson (Value (..))
import HQ.Optic
import HQ.Query (Query (..))
import HQ.Query.Parser (parseQuery)
import HQ.Transformation hiding (Compose)
import HQ.Transformation qualified as T
import Relude hiding (Compose, Const)
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
      parseQuery "fold @foo" `shouldBe` Right (Fold (Field "foo"))

    it "parses fold with each" $ do
      parseQuery "fold each" `shouldBe` Right (Fold Each)

    it "parses fold with composed optics" $ do
      let expected = Fold (Compose (Compose (Field "foo") Each) (Field "bar"))
      parseQuery "fold @foo.each.@bar" `shouldBe` Right expected

  describe "each optic" $ do
    it "parses each standalone" $ do
      parseQuery "fold each" `shouldBe` Right (Fold Each)

    it "parses each in composition" $ do
      let expected = Preview $ Compose (Field "foo") Each
      parseQuery "preview @foo.each" `shouldBe` Right expected

    it "parses each at the start of composition" $ do
      let expected = Preview $ Compose Each (Field "foo")
      parseQuery "preview each.@foo" `shouldBe` Right expected

  describe "field optic" $ do
    it "parses a simple field" $ do
      parseQuery "preview @name" `shouldBe` Right (Preview (Field "name"))

    it "parses a field with underscores" $ do
      parseQuery "fold @my_field" `shouldBe` Right (Fold (Field "my_field"))

    it "parses a field with numbers" $ do
      parseQuery "preview @field123" `shouldBe` Right (Preview (Field "field123"))

    it "parses a field with mixed alphanumeric and underscores" $ do
      parseQuery "preview @foo_bar_1" `shouldBe` Right (Preview (Field "foo_bar_1"))

  describe "id optic" $ do
    it "parses id standalone" $ do
      parseQuery "fold id" `shouldBe` Right (Fold Id)

    it "parses id in composition" $ do
      let composed = Preview $ Compose Id (Field "foo")
      parseQuery "preview id.@foo" `shouldBe` Right composed

  describe "keys/values/ix optics" $ do
    it "parses keys standalone" $ do
      parseQuery "fold keys" `shouldBe` Right (Fold Keys)

    it "parses values standalone" $ do
      parseQuery "fold values" `shouldBe` Right (Fold Values)

    it "parses keys in composition" $ do
      parseQuery "preview @obj.keys"
        `shouldBe` Right (Preview (Compose (Field "obj") Keys))

    it "parses keys with a prism" $ do
      parseQuery "preview keys._String"
        `shouldBe` Right (Preview (Compose Keys $ Prism PString))

    it "parses values in composition" $ do
      parseQuery "fold values.@x"
        `shouldBe` Right (Fold (Compose Values (Field "x")))

    it "parses ix with an index" $ do
      parseQuery "fold ix 10" `shouldBe` Right (Fold (Ix 10))

    it "parses filter with single words" $ do
      parseQuery "fold filter @age == 30"
        `shouldBe` Right (Fold (Filter (Field "age") (Equal (Number 30))))

    it "parses filter with parens" $ do
      let inner = Filter (Compose Each (Field "age")) (Equal (Number 30))
          expected = Fold (Compose (Field "users") inner)
      parseQuery "fold @users.filter (each . @age == 30)" `shouldBe` Right expected

    it "parses filter mid-path with a trailing transformation" $ do
      let inner = Filter (Field "age") (Equal (Number 30))
          expected = Over (Compose Each inner) (Add 1)
      parseQuery "over each.filter @age == 30 +1" `shouldBe` Right expected

    it "tolerates redundant parentheses around optics" $ do
      parseQuery "fold (@foo)" `shouldBe` Right (Fold (Field "foo"))
      parseQuery "fold ((@foo))" `shouldBe` Right (Fold (Field "foo"))
      parseQuery "fold (@foo . @bar)"
        `shouldBe` Right (Fold (Compose (Field "foo") (Field "bar")))

    it "tolerates redundant parentheses around filter sides" $ do
      let expected = Fold (Filter (Field "age") (Equal (Number 30)))
      parseQuery "fold filter (@age) (== 30)" `shouldBe` Right expected

    it "rejects empty parentheses" $ case parseQuery "fold ()" of
      Left _ -> pure ()
      Right q -> expectationFailure $ "Expected parse error, got: " <> show q

    it "parses over with keys" $ do
      parseQuery "over keys trim" `shouldBe` Right (Over Keys Trim)

    it "parses delete with values" $ do
      parseQuery "delete values" `shouldBe` Right (Delete Values)

  describe "whitespace handling" $ do
    it "handles extra whitespace around the query" $ do
      parseQuery "  preview @foo  " `shouldBe` Right (Preview (Field "foo"))

    it "handles whitespace around the dot separator" $ do
      let expected = Preview $ Compose (Field "foo") (Field "bar")
      parseQuery "preview @foo . @bar" `shouldBe` Right expected

    it "handles no whitespace" $ do
      parseQuery "preview@foo" `shouldBe` Right (Preview (Field "foo"))

    it "handles tabs" $ do
      parseQuery "\tpreview\t@foo\t" `shouldBe` Right (Preview (Field "foo"))

  describe "error cases" $ do
    it "rejects empty input" $ case parseQuery "" of
      Left _ -> pure ()
      Right q -> expectationFailure $ "Expected parse error, got: " <> show q

    it "rejects unknown operation" $ case parseQuery "unknown @foo" of
      Left _ -> pure ()
      Right q -> expectationFailure $ "Expected parse error, got: " <> show q

    it "rejects missing optic after operation" $ case parseQuery "fold" of
      Left _ -> pure ()
      Right q -> expectationFailure $ "Expected parse error, got: " <> show q

    it "rejects field without at" $ case parseQuery "fold foo" of
      Left _ -> pure ()
      Right q -> expectationFailure $ "Expected parse error, got: " <> show q

    it "rejects at without identifier" $ case parseQuery "fold @" of
      Left _ -> pure ()
      Right q -> expectationFailure $ "Expected parse error, got: " <> show q

    it "rejects command keyword prefixes" $ case parseQuery "folder @foo" of
      Left _ -> pure ()
      Right q -> expectationFailure $ "Expected parse error, got: " <> show q

    it "rejects optic keyword prefixes" $ case parseQuery "fold eachx" of
      Left _ -> pure ()
      Right q -> expectationFailure $ "Expected parse error, got: " <> show q

    it "rejects spaceless ix operand" $ case parseQuery "fold ix0" of
      Left _ -> pure ()
      Right q -> expectationFailure $ "Expected parse error, got: " <> show q

prismParserSpec :: Spec
prismParserSpec = describe "parseQuery (prisms)" $ do
  it "parses _String" $ do
    parseQuery "fold _String" `shouldBe` Right (Fold $ Prism PString)

  it "parses _Number" $ do
    parseQuery "fold _Number" `shouldBe` Right (Fold $ Prism PNumber)

  it "parses _Bool" $ do
    parseQuery "fold _Bool" `shouldBe` Right (Fold $ Prism PBool)

  it "parses _Null" $ do
    parseQuery "fold _Null" `shouldBe` Right (Fold $ Prism PNull)

  it "parses _Array" $ do
    parseQuery "fold _Array" `shouldBe` Right (Fold $ Prism PArray)

  it "parses _Object" $ do
    parseQuery "fold _Object" `shouldBe` Right (Fold $ Prism PObject)

  it "parses _Just" $ parseQuery "fold _Just" `shouldBe` Right (Fold PrismJust)

  it "parses prism in composition with each" $ do
    parseQuery "fold each._String"
      `shouldBe` Right (Fold (Compose Each $ Prism PString))

  it "parses prism in composition with field" $ do
    parseQuery "fold @data._Number"
      `shouldBe` Right (Fold (Compose (Field "data") $ Prism PNumber))

  it "parses prism composed with an index" $ do
    parseQuery "fold _Array.ix 0" `shouldBe` Right (Fold (Compose (Prism PArray) (Ix 0)))

overParserSpec :: Spec
overParserSpec = describe "parseQuery (over)" $ do
  it "parses over with a field" $ do
    parseQuery "over @foo +1" `shouldBe` Right (Over (Field "foo") (Add 1))

  it "parses over with each" $ do
    parseQuery "over each trim" `shouldBe` Right (Over Each Trim)

  it "parses over with composed optics" $ do
    parseQuery "over @foo.each +1"
      `shouldBe` Right (Over (Compose (Field "foo") Each) (Add 1))

  it "parses over with a composed transformation" $ do
    parseQuery "over @n +1 . == 3"
      `shouldBe` Right (Over (Field "n") (T.Compose (Add 1) (Equal (Number 3))))

  it "parses over with a string concatenation" $ do
    parseQuery "over @title ++\"!\""
      `shouldBe` Right (Over (Field "title") (ConcatString "!"))

  it "parses over with no whitespace" $ do
    parseQuery "over@foo+1" `shouldBe` Right (Over (Field "foo") (Add 1))

  it "rejects over without a transformation" $ case parseQuery "over @foo" of
    Left _ -> pure ()
    Right q -> expectationFailure $ "Expected parse error, got: " <> show q

setParserSpec :: Spec
setParserSpec = describe "parseQuery (set)" $ do
  it "parses set as a constant over" $ do
    parseQuery "set @name \"bob\""
      `shouldBe` Right (Over (Field "name") (Const (String "bob")))

  it "parses set with a number value" $ do
    parseQuery "set each 0" `shouldBe` Right (Over Each (Const (Number 0)))

  it "parses set with a composed optic" $ do
    parseQuery "set @users.each.@name \"anon\""
      `shouldBe` Right
        ( Over
            (Compose (Compose (Field "users") Each) (Field "name"))
            (Const (String "anon"))
        )
