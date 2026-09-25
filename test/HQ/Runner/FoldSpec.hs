{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner.FoldSpec (spec) where

import HQ.JSON.Decoder (initialDecoder)
import HQ.JSON.Event (JSONEvent (..), valueToEvents)
import HQ.JSON.Parser (parseValue)
import HQ.Optic (Optic)
import HQ.Optic.Parser (parseOptic)
import HQ.Runner.Cursor (Cursor (..))
import HQ.Runner.Fold (focusMany, runFold, runPreview)
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
  keysSpec
  valuesSpec
  ixSpec
  filterSpec
  differentialSpec
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

  it "returns the first array element with ix 0" $ do
    runQueryPreviewTest "ix 0" "[10,20]" `shouldReturn` Right [JSONNumber 10]

  it "returns nothing when ix 0 finds no element" $ do
    runQueryPreviewTest "ix 0" "[]" `shouldReturn` Right []

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
-- keys
--------------------------------------------------------------------------------

keysSpec :: Spec
keysSpec = describe "keys" $ do
  it "yields object keys as strings" $ do
    runQueryTest "keys" "{\"a\":1,\"b\":2}"
    `shouldReturn` Right [JSONString "a", JSONString "b"]

  it "yields nothing for arrays" $ do
    runQueryTest "keys" "[10,20,30]" `shouldReturn` Right []

  it "yields nothing for empty containers" $ do
    runQueryTest "keys" "{}" `shouldReturn` Right []
    runQueryTest "keys" "[]" `shouldReturn` Right []

  it "yields nothing for scalars" $ do
    runQueryTest "keys" "42" `shouldReturn` Right []
    runQueryTest "keys" "\"x\"" `shouldReturn` Right []

  it "composes with prisms" $ do
    runQueryTest "keys . _String" "{\"a\":1}" `shouldReturn` Right [JSONString "a"]
    runQueryTest "keys . _Number" "{\"a\":1}" `shouldReturn` Right []

  it "composes after a field" $ do
    runQueryTest "#obj . keys" "{\"obj\":{\"a\":1}}"
    `shouldReturn` Right [JSONString "a"]

  it "previews the first key" $ do
    runQueryPreviewTest "keys" "{\"a\":1,\"b\":2}"
    `shouldReturn` Right [JSONString "a"]

  it "previews nothing for arrays" $ do
    runQueryPreviewTest "keys" "[10,20]" `shouldReturn` Right []

--------------------------------------------------------------------------------
-- values (objects only)
--------------------------------------------------------------------------------

valuesSpec :: Spec
valuesSpec = describe "values" $ do
  it "yields object values" $ do
    runQueryTest "values" "{\"a\":1,\"b\":2}"
    `shouldReturn` Right [JSONNumber 1, JSONNumber 2]

  it "yields nothing for arrays" $ do
    runQueryTest "values" "[1,2,3]" `shouldReturn` Right []

  it "yields nothing for empty objects" $ do
    runQueryTest "values" "{}" `shouldReturn` Right []

  it "yields nothing for scalars" $ do
    runQueryTest "values" "42" `shouldReturn` Right []

  it "yields nested containers whole" $ do
    runQueryTest "values" "{\"a\":{\"b\":1}}"
    `shouldReturn` Right
      [ JSONBeginObject,
        JSONObjectKey "b",
        JSONNumber 1,
        JSONEndObject
      ]

  it "composes with a field" $ do
    runQueryTest "values . #x" "{\"a\":{\"x\":1},\"b\":{\"x\":2}}"
    `shouldReturn` Right [JSONNumber 1, JSONNumber 2]

  it "returns empty when the field is missing in all values" $ do
    runQueryTest "values . #x" "{\"a\":1}" `shouldReturn` Right []

  it "previews the first value" $ do
    runQueryPreviewTest "values" "{\"a\":1,\"b\":2}"
    `shouldReturn` Right [JSONNumber 1]

  it "previews nothing for arrays" $ do
    runQueryPreviewTest "values" "[1,2]" `shouldReturn` Right []

--------------------------------------------------------------------------------
-- ix
--------------------------------------------------------------------------------

ixSpec :: Spec
ixSpec = describe "ix" $ do
  it "yields the element at the index" $ do
    runQueryTest "ix 0" "[7,8]" `shouldReturn` Right [JSONNumber 7]
    runQueryTest "ix 1" "[7,8]" `shouldReturn` Right [JSONNumber 8]

  it "yields nothing when out of bounds" $ do
    runQueryTest "ix 5" "[1,2]" `shouldReturn` Right []

  it "yields nothing for objects" $ do
    runQueryTest "ix 0" "{\"a\":1}" `shouldReturn` Right []

  it "yields nothing for scalars" $ do
    runQueryTest "ix 0" "42" `shouldReturn` Right []

--------------------------------------------------------------------------------
-- filter
--------------------------------------------------------------------------------

filterSpec :: Spec
filterSpec = describe "filter" $ do
  it "keeps array elements when the test holds" $ do
    runQueryTest "each . filter #age == 30" "[{\"age\":30},{\"age\":20}]"
    `shouldReturn` Right [JSONBeginObject, JSONObjectKey "age", JSONNumber 30, JSONEndObject]

  it "keeps the whole value on any match" $ do
    runQueryTest "filter each == 1" "[1,2,1]"
    `shouldReturn` Right [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONNumber 1, JSONEndArray]

  it "drops values when nothing matches" $ do
    runQueryTest "each . filter #age == 30" "[{\"age\":20}]" `shouldReturn` Right []

  it "drops values with a missing focus" $ do
    runQueryTest "filter #age == 30" "{\"b\":1}" `shouldReturn` Right []

  it "filters scalars" $ do
    runQueryTest "filter id == 1" "1" `shouldReturn` Right [JSONNumber 1]
    runQueryTest "filter id == 1" "2" `shouldReturn` Right []

  it "composes after a filter" $ do
    runQueryTest "each . filter #age == 30 . #name" "[{\"age\":30,\"name\":\"a\"},{\"age\":20,\"name\":\"b\"}]"
    `shouldReturn` Right [JSONString "a"]

  it "keeps nested filtered values" $ do
    runQueryTest "filter (filter #a == 1 == {\"a\":1})" "{\"a\":1}"
    `shouldReturn` Right [JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject]

  it "fails when the predicate does not fit" $ do
    runQueryTest "each . filter #a +1" "[{\"a\":\"x\"}]" `shouldReturn` Left "expected a number"

  it "fails when the predicate is not boolean" $ do
    runQueryTest "each . filter #a +1" "[{\"a\":1}]" `shouldReturn` Left "filter transformation must produce a boolean"

  it "previews the first kept value" $ do
    runQueryPreviewTest "each . filter #age == 30" "[{\"age\":20},{\"age\":30}]"
    `shouldReturn` Right [JSONBeginObject, JSONObjectKey "age", JSONNumber 30, JSONEndObject]

  it "skips replayed containers looking for later members" $ do
    runQueryTest "filter #a == 1 . #zzz" "{\"a\":1,\"big\":[1,2,3]}" `shouldReturn` Right []

--------------------------------------------------------------------------------
-- pure vs streaming folds
--------------------------------------------------------------------------------

-- | The pure navigator ('focusMany') must agree with the streaming
-- fold as multisets: object member order differs (document order vs
-- key map order), and duplicate keys would collapse, so inputs have
-- unique keys and events compare order-insensitively.
differentialSpec :: Spec
differentialSpec = describe "pure vs streaming folds" $ do
  it "focusMany agrees with runFold as multisets" $ do
    forM_ differentialOptics $ \opticStr ->
      forM_ differentialDocs $ \doc ->
        case (parseOptic opticStr, parseValue doc) of
          (Right optic, Right value) -> do
            actual <- runFoldTest optic doc
            let expected = concatMap valueToEvents <$> focusMany optic value
            (sortOn (show :: JSONEvent -> String) <$> actual)
              `shouldBe` (sortOn (show :: JSONEvent -> String) <$> expected)
          (Left err, _) -> expectationFailure $ "bad optic: " <> show err
          (_, Left err) -> expectationFailure $ "bad doc: " <> show err

differentialOptics :: [Text]
differentialOptics =
  [ "id",
    "each",
    "keys",
    "values",
    "#a",
    "#missing",
    "ix 0",
    "ix 2",
    "_String",
    "_Number",
    "_Just",
    "_Null",
    "each . #x",
    "#a . each",
    "filter #a == 1",
    "filter (each . #x == 2)",
    "each . filter #b == 2"
  ]

differentialDocs :: [Text]
differentialDocs =
  [ "42",
    "\"hi\"",
    "true",
    "null",
    "[]",
    "[1,\"a\",true]",
    "{}",
    "{\"a\":1,\"b\":[2,3]}",
    "{\"a\":{\"x\":1},\"b\":2}",
    "[[1,2],[3]]"
  ]

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
    ("#a.#b", "{\"a\":{\"b\":[1,{\"c\":2}]}}"),
    ("keys", "{\"a\":1,\"b\":2}"),
    ("keys", "[10,20,30]"),
    ("keys . _String", "{\"a\":1}"),
    ("values", "{\"a\":1,\"b\":2}"),
    ("values . #x", "{\"a\":{\"x\":1},\"b\":2}"),
    ("ix 0", "{\"a\":1}"),
    ("each . filter #age == 30", "[{\"age\":30},{\"age\":20}]"),
    ("filter each == 1", "[1,2,1]"),
    ("each . filter #age == 30 . #name", "[{\"age\":30,\"name\":\"a\"}]")
  ]

malformedChunkCases :: [(Text, Text)]
malformedChunkCases =
  [ ("#b", "{\"a\":1,\"b\":tru}"),
    ("#a", "{\"a\":1,\"b\":tru}"),
    ("each", "[1,,2]"),
    ("#a", "{\"a\":01}"),
    ("keys", "[1,,2]"),
    ("values", "{\"a\":tru}"),
    ("filter #a == 1", "{\"a\":1,\"b\":tru}")
  ]
