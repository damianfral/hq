{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.JSON.ParserSpec (spec) where

import Data.Aeson (Value (..))
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KeyMap
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
  it "parses whole numbers" $ do
    parseValue "42" `shouldBe` Right (Number 42)
  it "parses negative numbers" $ do
    parseValue "-7" `shouldBe` Right (Number (-7))
  it "parses decimal fractions" $ do
    parseValue "0.5" `shouldBe` Right (Number 0.5)
  it "parses exponent notation" $ do
    parseValue "1e10" `shouldBe` Right (Number 1e10)
  it "parses signed exponent notation" $ do
    parseValue "-2.5e-3" `shouldBe` Right (Number (-2.5e-3))
  it "rejects non-numeric input" $ do
    parseValue "one" `shouldSatisfy` isLeft
  it "parses the full escape table" $ do
    parseValue "\"\\\"\\\\\\/\\b\\f\\n\\r\\t\""
      `shouldBe` Right (String "\"\\/\b\f\n\r\t")
  it "parses unicode escapes" $ do
    parseValue "\"caf\\u00e9\"" `shouldBe` Right (String "café")
  it "parses surrogate pairs" $ do
    parseValue "\"\\uD83D\\uDE00\"" `shouldBe` Right (String "\x1F600")
  it "parses escaped object keys" $ do
    parseValue "{\"k\\u0041\":1}"
      `shouldBe` Right (Object (KeyMap.fromList [(fromText "kA", Number 1)]))
  it "rejects invalid escapes" $ do
    parseValue "\"\\x\"" `shouldSatisfy` isLeft
  it "rejects lone surrogates"
    $ do
      parseValue "\"\\uD83D\"" `shouldSatisfy` isLeft
      parseValue "\"\\uDE00\"" `shouldSatisfy` isLeft
      parseValue "\"\\uD83Dx\"" `shouldSatisfy` isLeft
  it "rejects raw control characters" $ do
    parseValue "\"a\SOHb\"" `shouldSatisfy` isLeft

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
