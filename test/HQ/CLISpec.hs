{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.CLISpec (spec) where

import Data.Aeson (Value (..))
import HQ.CLI
import HQ.JSON.Encoder (EncodeStyle (..), Join (..), Raw (..))
import HQ.Optic (compose, each, field)
import HQ.Query (Query (Delete, Fold, Over, Preview))
import HQ.Transformation (constValue)
import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure)
import Relude hiding (Compose, id)
import Test.Syd

spec :: Spec
spec = do
  describe "command parsing" $ do
    let parseCLI :: [String] -> Either Text CLIOptions
        parseCLI args =
          case execParserPure defaultPrefs optParserInfo args of
            Success opts -> Right opts
            Failure _ -> Left "parse failure"
            CompletionInvoked _ -> Left "completion invoked"

    it "parses preview with a field optic" $ do
      optQuery <$> parseCLI ["preview", "@name"]
      `shouldBe` Right (Preview (field "name"))

    it "parses preview with each" $ do
      optQuery <$> parseCLI ["preview", "each"]
      `shouldBe` Right (Preview each)

    it "parses preview with a composed optic" $ do
      optQuery <$> parseCLI ["preview", "@a . @b"]
      `shouldBe` Right (Preview (compose (field "a") (field "b")))

    it "parses fold" $ do
      optQuery <$> parseCLI ["fold", "each"]
      `shouldBe` Right (Fold each)

    it "combines preview with compact"
      $ case parseCLI ["preview", "@name", "-c"] of
        Left err -> expectationFailure (toString err)
        Right opts -> do
          optQuery opts `shouldBe` Preview (field "name")
          optCompact opts `shouldBe` Compact

    it "combines preview with raw and join" $ do
      case parseCLI ["preview", "@name", "-r", "-j"] of
        Left err -> expectationFailure (toString err)
        Right opts -> do
          optQuery opts `shouldBe` Preview (field "name")
          optRaw opts `shouldBe` Raw
          optJoin opts `shouldBe` Join

    it "parses set with a field optic and value" $ do
      optQuery <$> parseCLI ["set", "@name", "\"bob\""]
      `shouldBe` Right (Over (field "name") (constValue (String "bob")))

    it "parses set with each and a number" $ do
      optQuery <$> parseCLI ["set", "each", "0"]
      `shouldBe` Right (Over each (constValue (Number 0)))

    it "parses set with a composed optic" $ do
      optQuery <$> parseCLI ["set", "@users.each.@name", "\"anon\""]
      `shouldBe` Right
        ( Over
            (compose (compose (field "users") each) (field "name"))
            (constValue (String "anon"))
        )

    it "parses delete with a field optic" $ do
      optQuery <$> parseCLI ["delete", "@name"]
      `shouldBe` Right (Delete (field "name"))

    it "parses delete with each" $ do
      optQuery <$> parseCLI ["delete", "each"]
      `shouldBe` Right (Delete each)

    it "combines set with compact" $ do
      case parseCLI ["set", "@name", "\"bob\"", "-c"] of
        Left err -> expectationFailure (toString err)
        Right opts -> do
          optQuery opts `shouldBe` Over (field "name") (constValue (String "bob"))
          optCompact opts `shouldBe` Compact
