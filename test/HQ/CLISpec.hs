{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.CLISpec (spec) where

import Control.Exception (try)
import Data.Aeson (Value (..))
import Data.Text qualified as T
import HQ.CLI
import HQ.JSON.Encoder (EncodeStyle (..), Join (..), Raw (..))
import HQ.Optic (Optic (..))
import HQ.Query (Query (Delete, Fold, Over, Preview))
import HQ.Transformation hiding (Compose)
import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure)
import Relude hiding (Compose, Const, id)
import System.Exit (ExitCode (..))
import System.IO.Error (doesNotExistErrorType, mkIOError)
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

    it "parses queries" $ forM_ queryCases $ \(args, expected) ->
      optQuery <$> parseCLI args `shouldBe` Right expected

    it "combines preview with compact"
      $ case parseCLI ["preview", "@name", "-c"] of
        Left err -> expectationFailure (toString err)
        Right opts -> do
          optQuery opts `shouldBe` Preview (Field "name")
          optCompact opts `shouldBe` Compact

    it "combines preview with raw and join" $ do
      case parseCLI ["preview", "@name", "-r", "-j"] of
        Left err -> expectationFailure (toString err)
        Right opts -> do
          optQuery opts `shouldBe` Preview (Field "name")
          optRaw opts `shouldBe` Raw
          optJoin opts `shouldBe` Join

    it "combines set with compact" $ do
      case parseCLI ["set", "@name", "\"bob\"", "-c"] of
        Left err -> expectationFailure (toString err)
        Right opts -> do
          optQuery opts `shouldBe` Over (Field "name") (Const (String "bob"))
          optCompact opts `shouldBe` Compact

  describe "withInput" $ do
    it "fails to process a non existent file" $ do
      -- 'withInput' reports the error itself and exits with failure;
      -- the handle body must never run.
      result <- try $ do
        withInput (Just "/nonexistent/hq-missing.json") $ const $ pure ()
      case result :: Either ExitCode () of
        Left code -> code `shouldNotBe` ExitSuccess
        Right () -> expectationFailure "expected the query to fail on an unopenable file"

queryCases :: [([String], Query)]
queryCases =
  [ (["preview", "@name"], Preview (Field "name")),
    (["preview", "each"], Preview Each),
    (["preview", "@a . @b"], Preview (Compose (Field "a") (Field "b"))),
    (["fold", "each"], Fold Each),
    (["set", "@name", "\"bob\""], Over (Field "name") (Const (String "bob"))),
    (["set", "each", "0"], Over Each (Const (Number 0))),
    ( ["set", "@users.each.@name", "\"anon\""],
      Over
        (Compose (Compose (Field "users") Each) (Field "name"))
        (Const (String "anon"))
    ),
    (["delete", "@name"], Delete (Field "name")),
    (["delete", "each"], Delete Each)
  ]
