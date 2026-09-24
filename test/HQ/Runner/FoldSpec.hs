{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner.FoldSpec (spec) where

import HQ.JSON.Decoder (initialDecoder)
import HQ.JSON.Event (JSONEvent (..))
import HQ.Optic (Optic)
import HQ.Optic.Parser (parseOptic)
import HQ.Runner.Cursor (Cursor (..))
import HQ.Runner.Fold (runFold, runPreview)
import Relude hiding (Compose, id, many, not, or, some, subtract, toStrict)
import Streaming (Of (..), Stream)
import qualified Streaming.Prelude as S
import Test.HQ (chunkSplits)
import Test.Syd

spec :: Spec
spec = describe "HQ.Runner.Fold" $ do
  eachArraySpec
  eachObjectSpec
  eachCompositionSpec
  fieldSpec
  idSpec
  previewSpec
  chunkedFoldSpec

-- | Run a fold optic against a JSON text input and collect output events.
--
-- This test harness builds a cursor from JSON text, runs the optic,
-- and collects all yielded events into a list. Runs in IO because the
-- streaming decoder and cursor use IO internally.
runFoldTest :: Optic -> Text -> IO (Either Text [JSONEvent])
runFoldTest optic input = runExceptT $ S.toList_ $ runFold optic cursor
  where
    textStream :: Stream (Of Text) (ExceptT Text IO) ()
    textStream = S.yield input
    cursor = Cursor [] initialDecoder textStream

-- | Parse an optic string and run it against JSON input.
runQueryTest :: Text -> Text -> IO (Either Text [JSONEvent])
runQueryTest opticStr jsonInput = case parseOptic opticStr of
  Left err -> pure (Left (show err))
  Right optic -> runFoldTest optic jsonInput

-- | Run a preview optic against a JSON text input and collect the
-- (at most one) output value's events.
runPreviewTest :: Optic -> Text -> IO (Either Text [JSONEvent])
runPreviewTest optic input = runExceptT $ S.toList_ $ runPreview optic cursor
  where
    textStream :: Stream (Of Text) (ExceptT Text IO) ()
    textStream = S.yield input
    cursor = Cursor [] initialDecoder textStream

-- | Parse an optic string and preview it against JSON input.
runQueryPreviewTest :: Text -> Text -> IO (Either Text [JSONEvent])
runQueryPreviewTest opticStr jsonInput = case parseOptic opticStr of
  Left err -> pure (Left (show err))
  Right optic -> runPreviewTest optic jsonInput

-- | Run a fold optic against chunked JSON text.
runFoldChunks :: Optic -> [Text] -> IO (Either Text [JSONEvent])
runFoldChunks optic chunks = runExceptT $ S.toList_ $ runFold optic cursor
  where
    textStream :: Stream (Of Text) (ExceptT Text IO) ()
    textStream = S.each chunks
    cursor = Cursor [] initialDecoder textStream

-- | Run a preview optic against chunked JSON text.
runPreviewChunks :: Optic -> [Text] -> IO (Either Text [JSONEvent])
runPreviewChunks optic chunks = runExceptT $ S.toList_ $ runPreview optic cursor
  where
    textStream :: Stream (Of Text) (ExceptT Text IO) ()
    textStream = S.each chunks
    cursor = Cursor [] initialDecoder textStream

--------------------------------------------------------------------------------
-- preview
--------------------------------------------------------------------------------

previewSpec :: Spec
previewSpec = describe "preview" $ do
  it "returns a field value" $ do
    runQueryPreviewTest "#name" "{\"name\":\"alice\",\"age\":30}"
    `shouldReturn` Right [JSONString "alice"]

  it "returns nothing when the field is missing" $ do
    runQueryPreviewTest "#name" "{\"age\":30}" `shouldReturn` Right []

  it "returns nothing for non-object input" $ do
    runQueryPreviewTest "#name" "42" `shouldReturn` Right []

  it "returns the first element of an array" $ do
    runQueryPreviewTest "each" "[1,2,3]" `shouldReturn` Right [JSONNumber 1]

  it "returns the first object value" $ do
    runQueryPreviewTest "each" "{\"a\":1,\"b\":2}"
    `shouldReturn` Right [JSONNumber 1]

  it "returns nothing for an empty array" $ do
    runQueryPreviewTest "each" "[]" `shouldReturn` Right []

  it "returns the first composed match" $ do
    runQueryPreviewTest "each . #name" "[{\"name\":\"alice\"},{\"name\":\"bob\"}]"
    `shouldReturn` Right [JSONString "alice"]

  it "returns the first array element with _1" $ do
    runQueryPreviewTest "_1" "[10,20]" `shouldReturn` Right [JSONNumber 10]

  it "returns nothing when _1 finds no element" $ do
    runQueryPreviewTest "_1" "[]" `shouldReturn` Right []

  it "returns nothing for an empty array" $ do
    runQueryPreviewTest "each" "[]" `shouldReturn` Right []

  it "returns the first composed match"
    $ runQueryPreviewTest "each . #name" "[{\"name\":\"alice\"},{\"name\":\"bob\"}]"
    `shouldReturn` Right [JSONString "alice"]

  it "returns the proper array element with ix" $ do
    runQueryPreviewTest "ix 0" "[0,1,2,3]" `shouldReturn` Right [JSONNumber 0]
    runQueryPreviewTest "ix 3" "[0,1,2,3]" `shouldReturn` Right [JSONNumber 3]

  it "returns nothing when ix finds no element" $ do
    runQueryPreviewTest "ix 0" "[]" `shouldReturn` Right []
    runQueryPreviewTest "ix 4" "[0,1,2,3]" `shouldReturn` Right []

  it "matches a string prism" $ do
    runQueryPreviewTest "_String" "\"hello\""
    `shouldReturn` Right [JSONString "hello"]

  it "returns nothing when a prism does not match" $ do
    runQueryPreviewTest "_String" "42" `shouldReturn` Right []

  it "returns a whole container value" $ do
    runQueryPreviewTest "#obj" "{\"obj\":{\"a\":1},\"next\":2}"
    `shouldReturn` Right
      [ JSONBeginObject,
        JSONObjectKey "a",
        JSONNumber 1,
        JSONEndObject
      ]

  it "returns the whole input for id" $ do
    runQueryPreviewTest "id" "[1,2]"
    `shouldReturn` Right [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONEndArray]

  it "stops reading input after the first match" $ do
    runQueryPreviewTest "each" "[1, 2,,]" `shouldReturn` Right [JSONNumber 1]

--------------------------------------------------------------------------------
-- field
--------------------------------------------------------------------------------

fieldSpec :: Spec
fieldSpec = describe "field" $ do
  it "extracts a field from an object" $ do
    runQueryTest "#name" "{\"name\":\"alice\"}"
    `shouldReturn` Right [JSONString "alice"]

  it "returns empty when field is missing" $ do
    runQueryTest "#name" "{\"age\":30}" `shouldReturn` Right []

  it "returns empty for non-object input" $ do
    runQueryTest "#name" "42" `shouldReturn` Right []

--------------------------------------------------------------------------------
-- id
--------------------------------------------------------------------------------

idSpec :: Spec
idSpec = describe "id" $ do
  it "returns the input value unchanged" $ do
    runQueryTest "id" "42" `shouldReturn` Right [JSONNumber 42]

  it "returns a string value unchanged" $ do
    runQueryTest "id" "\"hello\"" `shouldReturn` Right [JSONString "hello"]

--------------------------------------------------------------------------------
-- each on arrays
--------------------------------------------------------------------------------

eachArraySpec :: Spec
eachArraySpec = describe "each on arrays" $ do
  it "yields each element of a numeric array" $ do
    runQueryTest "each" "[1,2,3]"
    `shouldReturn` Right [JSONNumber 1, JSONNumber 2, JSONNumber 3]

  it "yields each element of a string array" $ do
    runQueryTest "each" "[\"a\",\"b\",\"c\"]"
    `shouldReturn` Right [JSONString "a", JSONString "b", JSONString "c"]

  it "yields a single-element array" $ do
    runQueryTest "each" "[42]" `shouldReturn` Right [JSONNumber 42]

  it "yields nothing for an empty array" $ do
    runQueryTest "each" "[]" `shouldReturn` Right []

  it "yields nothing for non-container input" $ do
    runQueryTest "each" "42" `shouldReturn` Right []

  it "yields nested array elements" $ do
    runQueryTest "each" "[[1,2],[3]]"
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
  it "yields each value of an object" $ do
    runQueryTest "each" "{\"a\":1,\"b\":2}"
    `shouldReturn` Right [JSONNumber 1, JSONNumber 2]

  it "yields nothing for an empty object" $ do
    runQueryTest "each" "{}" `shouldReturn` Right []

--------------------------------------------------------------------------------
-- each composed with field
--------------------------------------------------------------------------------

eachCompositionSpec :: Spec
eachCompositionSpec = describe "each . field composition" $ do
  it "extracts field from each array element" $ do
    runQueryTest "each . #name" "[{\"name\":\"alice\"},{\"name\":\"bob\"}]"
    `shouldReturn` Right [JSONString "alice", JSONString "bob"]

  it "extracts field from each object value" $ do
    runQueryTest "each . #x" "{\"a\":{\"x\":1},\"b\":{\"x\":2}}"
    `shouldReturn` Right [JSONNumber 1, JSONNumber 2]

  it "returns empty when field is missing in some elements" $ do
    runQueryTest "each . #name" "[{\"name\":\"alice\"},{\"age\":30}]"
    `shouldReturn` Right [JSONString "alice"]

  it "returns empty when field missing in all elements" $ do
    runQueryTest "each . #name" "[{\"a\":1},{\"b\":2}]" `shouldReturn` Right []

  it "extracts field from each element with multiple fields" $ do
    runQueryTest "each . #id" "[{\"name\":\"alice\",\"id\":\"1\"},{\"name\":\"bob\",\"id\":\"2\"}]"
    `shouldReturn` Right [JSONString "1", JSONString "2"]

  it "field . each distributes field then each" $ do
    runQueryTest "#items . each" "{\"items\":[1,2,3]}"
    `shouldReturn` Right [JSONNumber 1, JSONNumber 2, JSONNumber 3]

  it "each . each flattens nested arrays" $ do
    runQueryTest "each . each" "[[1,2],[3,4]]"
    `shouldReturn` Right [JSONNumber 1, JSONNumber 2, JSONNumber 3, JSONNumber 4]

chunkedFoldSpec :: Spec
chunkedFoldSpec = describe "chunked input" $ do
  it "folds agree with whole-input runs under every split" $ do
    forM_ foldChunkCases $ \(opticStr, doc) ->
      case parseOptic opticStr of
        Left err -> expectationFailure $ "bad optic: " <> show err
        Right optic -> do
          expected <- runFoldTest optic doc
          forM_ (chunkSplits doc) $ \chunks -> do
            actual <- runFoldChunks optic chunks
            actual `shouldBe` expected

  it "preview never touches invalid tails under any split" $ do
    forM_ (chunkSplits "[1, 2,,]") $ \chunks -> do
      actual <- runPreviewChunks' "each" chunks
      actual `shouldBe` Right [JSONNumber 1]

  it "malformed inputs fail the same chunked as whole" $ do
    forM_ malformedChunkCases $ \(opticStr, doc) -> do
      expected <- runQueryTest opticStr doc
      case parseOptic opticStr of
        Left err -> expectationFailure $ "bad optic: " <> show err
        Right optic ->
          forM_ (chunkSplits doc) $ \chunks -> do
            actual <- runFoldChunks optic chunks
            actual `shouldBe` expected

-- | Parse an optic string and preview it against chunked JSON input.
runPreviewChunks' :: Text -> [Text] -> IO (Either Text [JSONEvent])
runPreviewChunks' opticStr chunks =
  case parseOptic opticStr of
    Left err -> pure (Left (show err))
    Right optic -> runPreviewChunks optic chunks

foldChunkCases :: [(Text, Text)]
foldChunkCases =
  [ ("each . #name", "[{\"name\":\"alice\"},{\"age\":30}]"),
    ("#users.each.#name", "{\"users\":[{\"name\":\"a\",\"age\":1},{\"name\":\"b\"}]}"),
    ("each", "[[1,2],[3]]"),
    ("#a", "{\"a\":{\"b\":[1,2]},\"c\":3}"),
    ("#missing", "{\"a\":1}"),
    ("ix 2", "[1,2,3,4]"),
    ("each . each", "[[1,2],[3,4]]"),
    ("#a.#b", "{\"a\":{\"b\":[1,{\"c\":2}]}}")
  ]

malformedChunkCases :: [(Text, Text)]
malformedChunkCases =
  [ ("#b", "{\"a\":1,\"b\":tru}"),
    ("#a", "{\"a\":1,\"b\":tru}"),
    ("each", "[1,,2]"),
    ("#a", "{\"a\":01}")
  ]
