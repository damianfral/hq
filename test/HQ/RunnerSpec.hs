{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.RunnerSpec (spec) where

import HQ.JSON.Cursor (fromStream)
import HQ.JSON.Decoder (decodeIO)
import HQ.JSON.Event (JSONEvent (..))
import HQ.Optic (Optic)
import HQ.Optic.Parser (parseOptic)
import HQ.Runner (runFold)
import Relude hiding (Compose, id)
import Streaming (Of (..), Stream)
import qualified Streaming.Prelude as S
import Test.Syd

-- | Run a fold optic against a JSON text input and collect output events.
--
-- This test harness builds a streaming cursor from JSON text, runs the optic,
-- and collects all yielded events into a list. Runs in IO because the
-- streaming decoder and cursor use IO internally.
runFoldTest :: Optic -> Text -> IO (Either Text [JSONEvent])
runFoldTest optic input = runExceptT $ do
  let textStream :: Stream (Of Text) (ExceptT Text IO) ()
      textStream = S.yield input
  let eventStream = decodeIO textStream
  let cursor = fromStream eventStream
  resultStream <- runFold optic cursor
  S.toList_ resultStream

-- | Parse an optic string and run it against JSON input.
runQueryTest :: Text -> Text -> IO (Either Text [JSONEvent])
runQueryTest opticStr jsonInput =
  case parseOptic opticStr of
    Left err -> pure (Left (show err))
    Right optic -> runFoldTest optic jsonInput

spec :: Spec
spec = describe "HQ.Runner" $ do
  eachArraySpec
  eachObjectSpec
  eachCompositionSpec
  fieldSpec
  idSpec

--------------------------------------------------------------------------------
-- field
--------------------------------------------------------------------------------

fieldSpec :: Spec
fieldSpec = describe "field" $ do
  it "extracts a field from an object"
    $ runQueryTest "#name" "{\"name\":\"alice\"}"
    `shouldReturn` Right [JSONString "alice"]

  it "returns empty when field is missing"
    $ runQueryTest "#name" "{\"age\":30}"
    `shouldReturn` Right []

  it "returns empty for non-object input"
    $ runQueryTest "#name" "42"
    `shouldReturn` Right []

--------------------------------------------------------------------------------
-- id
--------------------------------------------------------------------------------

idSpec :: Spec
idSpec = describe "id" $ do
  it "returns the input value unchanged"
    $ runQueryTest "id" "42"
    `shouldReturn` Right [JSONNumber 42]

  it "returns a string value unchanged"
    $ runQueryTest "id" "\"hello\""
    `shouldReturn` Right [JSONString "hello"]

--------------------------------------------------------------------------------
-- each on arrays
--------------------------------------------------------------------------------

eachArraySpec :: Spec
eachArraySpec = describe "each on arrays" $ do
  it "yields each element of a numeric array"
    $ runQueryTest "each" "[1,2,3]"
    `shouldReturn` Right [JSONNumber 1, JSONNumber 2, JSONNumber 3]

  it "yields each element of a string array"
    $ runQueryTest "each" "[\"a\",\"b\",\"c\"]"
    `shouldReturn` Right [JSONString "a", JSONString "b", JSONString "c"]

  it "yields a single-element array"
    $ runQueryTest "each" "[42]"
    `shouldReturn` Right [JSONNumber 42]

  it "yields nothing for an empty array"
    $ runQueryTest "each" "[]"
    `shouldReturn` Right []

  it "yields nothing for non-container input"
    $ runQueryTest "each" "42"
    `shouldReturn` Right []

  it "yields nested array elements"
    $ runQueryTest "each" "[[1,2],[3]]"
    `shouldReturn` Right
      [ JSONBeginArray,
        JSONNumber 1,
        JSONNumber 2,
        JSONEndArray,
        JSONBeginArray,
        JSONNumber 3,
        JSONEndArray
      ]

--------------------------------------------------------------------------------
-- each on objects
--------------------------------------------------------------------------------

eachObjectSpec :: Spec
eachObjectSpec = describe "each on objects" $ do
  it "yields each value of an object"
    $ runQueryTest "each" "{\"a\":1,\"b\":2}"
    `shouldReturn` Right [JSONNumber 1, JSONNumber 2]

  it "yields nothing for an empty object"
    $ runQueryTest "each" "{}"
    `shouldReturn` Right []

--------------------------------------------------------------------------------
-- each composed with field
--------------------------------------------------------------------------------

eachCompositionSpec :: Spec
eachCompositionSpec = describe "each . field composition" $ do
  it "extracts field from each array element"
    $ runQueryTest "each . #name" "[{\"name\":\"alice\"},{\"name\":\"bob\"}]"
    `shouldReturn` Right [JSONString "alice", JSONString "bob"]

  it "extracts field from each object value"
    $ runQueryTest "each . #x" "{\"a\":{\"x\":1},\"b\":{\"x\":2}}"
    `shouldReturn` Right [JSONNumber 1, JSONNumber 2]

  it "returns empty when field is missing in some elements"
    $ runQueryTest "each . #name" "[{\"name\":\"alice\"},{\"age\":30}]"
    `shouldReturn` Right [JSONString "alice"]

  it "returns empty when field missing in all elements"
    $ runQueryTest "each . #name" "[{\"a\":1},{\"b\":2}]"
    `shouldReturn` Right []

  it "extracts field from each element with multiple fields"
    $ runQueryTest "each . #id" "[{\"name\":\"alice\",\"id\":\"1\"},{\"name\":\"bob\",\"id\":\"2\"}]"
    `shouldReturn` Right [JSONString "1", JSONString "2"]

  it "field . each distributes field then each"
    $ runQueryTest "#items . each" "{\"items\":[1,2,3]}"
    `shouldReturn` Right [JSONNumber 1, JSONNumber 2, JSONNumber 3]

  it "each . each flattens nested arrays"
    $ runQueryTest "each . each" "[[1,2],[3,4]]"
    `shouldReturn` Right [JSONNumber 1, JSONNumber 2, JSONNumber 3, JSONNumber 4]
