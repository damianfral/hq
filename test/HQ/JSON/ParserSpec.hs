{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.JSON.ParserSpec (spec) where

import HQ.JSON.Event (JSONEvent (..))
import HQ.JSON.Parser (parseValueEvents)
import Relude hiding (Compose, id)
import Test.HQ (decodeStreaming)
import Test.Syd

spec :: Spec
spec = describe "HQ.JSON.Parser" parseValueEventsSpec

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
    case (parseValueEvents input, decodeStreaming [input]) of
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
