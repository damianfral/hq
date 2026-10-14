{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.JSON.ParserSpec (spec) where

import Data.Aeson (Value (..))
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KeyMap
import HQ.JSON.Event (JSONEvent (..))
import HQ.JSON.Parser (parseValue, parseValueEvents)
import Relude hiding (Compose, id)
import Test.HQ (decodeChunks)
import Test.Syd

spec :: Spec
spec = describe "HQ.JSON.Parser" $ do
  parseValueSpec
  parseValueEventsSpec

--------------------------------------------------------------------------------
-- parseValue
--------------------------------------------------------------------------------

parseValueSpec :: Spec
parseValueSpec = describe "parseValue" $ do
  it "parses values" $ forM_ valueCases $ \(input, expected) ->
    parseValue input `shouldBe` Right expected

  it "rejects invalid input" $ forM_ invalidCases $ \input ->
    parseValue input `shouldSatisfy` isLeft

valueCases :: [(Text, Value)]
valueCases =
  [ ("42", Number 42),
    ("-7", Number (-7)),
    ("0.5", Number 0.5),
    ("1e10", Number 1e10),
    ("-2.5e-3", Number (-2.5e-3)),
    ("\"\\\"\\\\\\/\\b\\f\\n\\r\\t\"", String "\"\\/\b\f\n\r\t"),
    ("\"caf\\u00e9\"", String "café"),
    ("\"\\uD83D\\uDE00\"", String "\x1F600"),
    ("{\"k\\u0041\":1}", Object (KeyMap.fromList [(fromText "kA", Number 1)]))
  ]

invalidCases :: [Text]
invalidCases =
  [ "one",
    "\"\\x\"",
    "\"\\uD83D\"",
    "\"\\uDE00\"",
    "\"\\uD83Dx\"",
    "\"a\SOHb\""
  ]

--------------------------------------------------------------------------------
-- parseValueEvents
--------------------------------------------------------------------------------

-- | Parse a value to events, or fail the test with the error message.
parseEventsOrFail :: Text -> IO [JSONEvent]
parseEventsOrFail input = case parseValueEvents input of
  Left err -> expectationFailure (toString err) >> pure []
  Right events -> pure events

parseValueEventsSpec :: Spec
parseValueEventsSpec = describe "parseValueEvents" $ do
  it "preserves object member order" $ do
    events <- parseEventsOrFail "{\"b\":1,\"a\":2}"
    events
      `shouldBe` [ JSONBeginObject,
                   JSONObjectKey "b",
                   JSONNumber 1,
                   JSONObjectKey "a",
                   JSONNumber 2,
                   JSONEndObject
                 ]

  it "produces the same events as the streaming decoder" $ do
    let input = "{\"users\":[{\"name\":\"alice\",\"age\":30}],\"count\":2}"
    case (parseValueEvents input, decodeChunks [input]) of
      (Left err, _) -> expectationFailure (toString err)
      (_, Left err) -> expectationFailure (show err)
      (Right events, Right decoded) -> events `shouldBe` decoded

  it "converts scalars, arrays and nested containers" $ do
    events <- parseEventsOrFail "[1,\"x\",true,null,{\"k\":[]}]"
    events
      `shouldBe` [ JSONBeginArray,
                   JSONNumber 1,
                   JSONString "x",
                   JSONBool True,
                   JSONNull,
                   JSONBeginObject,
                   JSONObjectKey "k",
                   JSONBeginArray,
                   JSONEndArray,
                   JSONEndObject,
                   JSONEndArray
                 ]

  it "rejects trailing input"
    $ parseValueEvents "[1,2] x"
    `shouldSatisfy` isLeft

  it "rejects malformed input"
    $ parseValueEvents "{\"a\":}"
    `shouldSatisfy` isLeft

  it "agrees with the streaming decoder on escapes" $ do
    let input = "{\"k\\u0041\":\"a\\tb\\/c\\uD83D\\uDE00\"}"
    case (parseValueEvents input, decodeChunks [input]) of
      (Left err, _) -> expectationFailure (toString err)
      (_, Left err) -> expectationFailure (show err)
      (Right events, Right decoded) -> events `shouldBe` decoded
