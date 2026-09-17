{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.CLISpec (spec) where

import HQ.CLI
  ( CLIOptions (..),
    Compact (..),
    Join (..),
    OutputConfig (..),
    Raw (..),
    optParserInfo,
    outputConfig,
  )
import qualified HQ.JSON.Encoder as Enc
import HQ.JSON.Event (JSONEvent (..))
import HQ.Optic (compose, each, field)
import HQ.Query (Query (Delete, Fold, Preview, Set))
import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure)
import Relude hiding (Compose, id)
import Test.Syd

spec :: Spec
spec = describe "outputConfig" $ do
  it "defaults to pretty style with newline separation"
    $ outputConfig NoCompact NoJoin NoRaw
    `shouldBe` OutputConfig (Enc.Pretty 2) (Enc.ValueOptions True False)

  it "compact selects compact style"
    $ outputConfig Compact NoJoin NoRaw
    `shouldBe` OutputConfig Enc.Compact (Enc.ValueOptions True False)

  it "join disables value separation"
    $ outputConfig NoCompact Join NoRaw
    `shouldBe` OutputConfig (Enc.Pretty 2) (Enc.ValueOptions False False)

  it "raw enables raw top-level strings"
    $ outputConfig NoCompact NoJoin Raw
    `shouldBe` OutputConfig (Enc.Pretty 2) (Enc.ValueOptions True True)

  it "combines compact, join and raw"
    $ outputConfig Compact Join Raw
    `shouldBe` OutputConfig Enc.Compact (Enc.ValueOptions False True)

  describe "command parsing" $ do
    let parseCLI :: [String] -> Either Text CLIOptions
        parseCLI args =
          case execParserPure defaultPrefs optParserInfo args of
            Success opts -> Right opts
            Failure _ -> Left "parse failure"
            CompletionInvoked _ -> Left "completion invoked"
    it "parses preview with a field optic"
      $ optQuery
      <$> parseCLI ["preview", "#name"]
      `shouldBe` Right (Preview (field "name"))
    it "parses preview with each"
      $ optQuery
      <$> parseCLI ["preview", "each"]
      `shouldBe` Right (Preview each)
    it "parses preview with a composed optic"
      $ optQuery
      <$> parseCLI ["preview", "#a . #b"]
      `shouldBe` Right (Preview (compose (field "a") (field "b")))
    it "still parses fold"
      $ optQuery
      <$> parseCLI ["fold", "each"]
      `shouldBe` Right (Fold each)
    it "combines preview with compact" $ do
      case parseCLI ["preview", "#name", "-c"] of
        Left err -> expectationFailure (toString err)
        Right opts -> do
          optQuery opts `shouldBe` Preview (field "name")
          optCompact opts `shouldBe` Compact
    it "combines preview with raw and join" $ do
      case parseCLI ["preview", "#name", "-r", "-j"] of
        Left err -> expectationFailure (toString err)
        Right opts -> do
          optQuery opts `shouldBe` Preview (field "name")
          optRaw opts `shouldBe` Raw
          optJoin opts `shouldBe` Join
    it "parses set with a field optic and value"
      $ optQuery
      <$> parseCLI ["set", "#name", "\"bob\""]
      `shouldBe` Right (Set (field "name") [JSONString "bob"])
    it "parses set with each and a number"
      $ optQuery
      <$> parseCLI ["set", "each", "0"]
      `shouldBe` Right (Set each [JSONNumber 0])
    it "parses set with a composed optic"
      $ optQuery
      <$> parseCLI ["set", "#users.each.#name", "\"anon\""]
      `shouldBe` Right (Set (compose (compose (field "users") each) (field "name")) [JSONString "anon"])
    it "parses delete with a field optic"
      $ optQuery
      <$> parseCLI ["delete", "#name"]
      `shouldBe` Right (Delete (field "name"))
    it "parses delete with each"
      $ optQuery
      <$> parseCLI ["delete", "each"]
      `shouldBe` Right (Delete each)
    it "combines set with compact" $ do
      case parseCLI ["set", "#name", "\"bob\"", "-c"] of
        Left err -> expectationFailure (toString err)
        Right opts -> do
          optQuery opts `shouldBe` Set (field "name") [JSONString "bob"]
          optCompact opts `shouldBe` Compact
