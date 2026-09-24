{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.RunnerSpec (spec) where

import Control.Monad.Error.Class (throwError)
import Data.Aeson (Value (..))
import qualified Data.Text as T
import HQ.JSON.Decoder (initialDecoder)
import HQ.JSON.Encoder (EncodeStyle (..), EncoderConfig (..), Join (..), Raw (..), ValueOptions (..), encodeChunks)
import HQ.JSON.Event (JSONEvent (..), eventsToValue)
import HQ.JSON.Parser (parseValueEvents)
import HQ.Optic (Optic)
import HQ.Optic.Parser (parseOptic)
import HQ.Runner (Cursor (..), KSplice, runDelete, runFold, runOver, runPreview)
import HQ.Transformation (Transformation, add, combine, concatString, constValue, equal, not, or, replace, trim)
import Relude hiding (Compose, id, many, not, or, some, subtract, toStrict)
import Streaming (Of (..), Stream)
import qualified Streaming.Prelude as S
import Test.Syd

-- | Run a fold optic against a JSON text input and collect output events.
--
-- This test harness builds a cursor from JSON text, runs the optic,
-- and collects all yielded events into a list. Runs in IO because the
-- streaming decoder and cursor use IO internally.
runFoldTest :: Optic -> Text -> IO (Either Text [JSONEvent])
runFoldTest optic input = runExceptT $ do
  let textStream :: Stream (Of Text) (ExceptT Text IO) ()
      textStream = S.yield input
  let cursor = Cursor [] initialDecoder textStream
  S.toList_ (runFold optic cursor)

-- | Parse an optic string and run it against JSON input.
runQueryTest :: Text -> Text -> IO (Either Text [JSONEvent])
runQueryTest opticStr jsonInput =
  case parseOptic opticStr of
    Left err -> pure (Left (show err))
    Right optic -> runFoldTest optic jsonInput

-- | Run a preview optic against a JSON text input and collect the
-- (at most one) output value's events.
runPreviewTest :: Optic -> Text -> IO (Either Text [JSONEvent])
runPreviewTest optic input = runExceptT $ do
  let textStream :: Stream (Of Text) (ExceptT Text IO) ()
      textStream = S.yield input
  let cursor = Cursor [] initialDecoder textStream
  S.toList_ (runPreview optic cursor)

-- | Parse an optic string and preview it against JSON input.
runQueryPreviewTest :: Text -> Text -> IO (Either Text [JSONEvent])
runQueryPreviewTest opticStr jsonInput =
  case parseOptic opticStr of
    Left err -> pure (Left (show err))
    Right optic -> runPreviewTest optic jsonInput

-- | Run a fold optic against chunked JSON text.
runFoldChunks :: Optic -> [Text] -> IO (Either Text [JSONEvent])
runFoldChunks optic chunks = runExceptT $ do
  let textStream :: Stream (Of Text) (ExceptT Text IO) ()
      textStream = S.each chunks
  let cursor = Cursor [] initialDecoder textStream
  S.toList_ (runFold optic cursor)

-- | Run a preview optic against chunked JSON text.
runPreviewChunks :: Optic -> [Text] -> IO (Either Text [JSONEvent])
runPreviewChunks optic chunks = runExceptT $ do
  let textStream :: Stream (Of Text) (ExceptT Text IO) ()
      textStream = S.each chunks
  let cursor = Cursor [] initialDecoder textStream
  S.toList_ (runPreview optic cursor)

-- | Encoder config for rewrite tests: pretty output, matching the
-- production default.
testConfig :: EncoderConfig
testConfig = EncoderConfig (Pretty 2) (ValueOptions NoRaw NoJoin)

-- | Run a document-rewriting query (set/delete), reparse its output
-- bytes back to events, and collect them. Reparsing through the
-- independent pure parser keeps every existing event expectation
-- valid while the splice pipeline emits chunks.
runRewriteTest ::
  KSplice ->
  Text ->
  IO (Either Text [JSONEvent])
runRewriteTest run input = runRewriteChunks run [input]

-- | Run a document-rewriting query against chunked JSON text.
runRewriteChunks ::
  KSplice ->
  [Text] ->
  IO (Either Text [JSONEvent])
runRewriteChunks run chunks = runExceptT $ do
  let textStream :: Stream (Of Text) (ExceptT Text IO) ()
      textStream = S.each chunks
  let cursor = Cursor [] initialDecoder textStream
  (outChunks :> _) <- S.toList (run cursor [])
  (byteChunks :> _) <- S.toList (encodeChunks 65536 (S.each outChunks))
  let text = decodeUtf8 (mconcat byteChunks)
  -- No output bytes means no values (e.g. deleting the whole
  -- document): nothing to reparse.
  if T.null (T.strip text)
    then pure []
    else case parseValueEvents text of
      Left err -> throwError err
      Right events -> pure events

-- | Chunkings: whole input, every two-way split, and one-character
-- chunks for short inputs.
chunkSplits :: Text -> [[Text]]
chunkSplits input
  | T.null input = [[input]]
  | otherwise =
      [input]
        : [[T.take n input, T.drop n input] | n <- [1 .. T.length input - 1]]
          ++ [[T.singleton c | c <- toString input] | T.length input <= 24]

-- | Parse an optic string and a replacement value, run @set@ against
-- JSON input, and collect the rewritten document's events.
--
-- @set@ is @over@ with a constant transformation, so the harness drives
-- 'runOver' with 'constValue'.
runSetTest :: Text -> Text -> Text -> IO (Either Text [JSONEvent])
runSetTest opticStr valueStr jsonInput =
  case parseOptic opticStr of
    Left err -> pure (Left (show err))
    Right optic -> case parseValueEvents valueStr >>= eventsToValue of
      Left err -> pure (Left err)
      Right value -> runRewriteTest (runOver optic (constValue value) testConfig) jsonInput

-- | Parse an optic string and run @delete@ against JSON input,
-- collecting the rewritten document's events.
runDeleteTest :: Text -> Text -> IO (Either Text [JSONEvent])
runDeleteTest opticStr jsonInput =
  case parseOptic opticStr of
    Left err -> pure (Left (show err))
    Right optic -> runRewriteTest (runDelete optic testConfig) jsonInput

-- | Parse an optic string and a transformation, run @over@ against JSON
-- input, and collect the rewritten document's events.
runOverTest :: Text -> Transformation -> Text -> IO (Either Text [JSONEvent])
runOverTest opticStr transformation jsonInput =
  case parseOptic opticStr of
    Left err -> pure (Left (show err))
    Right optic -> runRewriteTest (runOver optic transformation testConfig) jsonInput

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
-- set
--------------------------------------------------------------------------------

setSpec :: Spec
setSpec = describe "set" $ do
  it "replaces a field value" $ do
    runSetTest "#name" "\"bob\"" "{\"name\":\"alice\",\"age\":30}"
    `shouldReturn` Right
      [ JSONBeginObject,
        JSONObjectKey "name",
        JSONString "bob",
        JSONObjectKey "age",
        JSONNumber 30,
        JSONEndObject
      ]

  it "leaves the document unchanged when the field is missing" $ do
    runSetTest "#name" "5" "{\"age\":30}"
    `shouldReturn` Right
      [JSONBeginObject, JSONObjectKey "age", JSONNumber 30, JSONEndObject]

  it "leaves non-object input unchanged" $ do
    runSetTest "#name" "5" "[1,2]"
    `shouldReturn` Right [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONEndArray]

  it "replaces every array element" $ do
    runSetTest "each" "0" "[1,2,3]"
    `shouldReturn` Right
      [JSONBeginArray, JSONNumber 0, JSONNumber 0, JSONNumber 0, JSONEndArray]

  it "replaces every object value" $ do
    runSetTest "each" "true" "{\"a\":1,\"b\":2}"
    `shouldReturn` Right
      [ JSONBeginObject,
        JSONObjectKey "a",
        JSONBool True,
        JSONObjectKey "b",
        JSONBool True,
        JSONEndObject
      ]

  it "leaves an empty array unchanged" $ do
    runSetTest "each" "0" "[]"
    `shouldReturn` Right [JSONBeginArray, JSONEndArray]

  it "leaves an empty object unchanged" $ do
    runSetTest "each" "0" "{}"
    `shouldReturn` Right [JSONBeginObject, JSONEndObject]

  it "replaces only strings with a prism" $ do
    runSetTest "each . _String" "\"x\"" "[1,\"a\",true]"
    `shouldReturn` Right
      [ JSONBeginArray,
        JSONNumber 1,
        JSONString "x",
        JSONBool True,
        JSONEndArray
      ]

  it "passes non-matching values through unchanged" $ do
    runSetTest "_String" "\"x\"" "5" `shouldReturn` Right [JSONNumber 5]

  it "replaces fields in a composed traversal" $ do
    runSetTest "#users.each.#name" "\"anon\"" "{\"users\":[{\"name\":\"a\",\"age\":1},{\"name\":\"b\"}]}"
    `shouldReturn` Right
      [ JSONBeginObject,
        JSONObjectKey "users",
        JSONBeginArray,
        JSONBeginObject,
        JSONObjectKey "name",
        JSONString "anon",
        JSONObjectKey "age",
        JSONNumber 1,
        JSONEndObject,
        JSONBeginObject,
        JSONObjectKey "name",
        JSONString "anon",
        JSONEndObject,
        JSONEndArray,
        JSONEndObject
      ]

  it "keeps members unchanged when the composed target is missing" $ do
    runSetTest "#users.each.#name" "\"anon\"" "{\"users\":[{\"age\":1}]}"
    `shouldReturn` Right
      [ JSONBeginObject,
        JSONObjectKey "users",
        JSONBeginArray,
        JSONBeginObject,
        JSONObjectKey "age",
        JSONNumber 1,
        JSONEndObject,
        JSONEndArray,
        JSONEndObject
      ]

  it "replaces the first array element" $ do
    runSetTest "_1" "9" "[[1,2],[3]]"
    `shouldReturn` Right
      [ JSONBeginArray,
        JSONNumber 9,
        JSONBeginArray,
        JSONNumber 3,
        JSONEndArray,
        JSONEndArray
      ]

  it "replaces the second array element" $ do
    runSetTest "_2" "9" "[1,[2,3]]"
    `shouldReturn` Right
      [JSONBeginArray, JSONNumber 1, JSONNumber 9, JSONEndArray]

  it "replaces the whole document with id" $ do
    runSetTest "id" "5" "[1,2]" `shouldReturn` Right [JSONNumber 5]

  it "replaces the whole document with a compound value" $ do
    runSetTest "id" "{\"x\":1}" "5"
    `shouldReturn` Right
      [JSONBeginObject, JSONObjectKey "x", JSONNumber 1, JSONEndObject]

  it "replaces a nested container value wholesale"
    $ runSetTest "#a" "5" "{\"a\":{\"b\":[1,2]},\"c\":3}"
    `shouldReturn` Right
      [ JSONBeginObject,
        JSONObjectKey "a",
        JSONNumber 5,
        JSONObjectKey "c",
        JSONNumber 3,
        JSONEndObject
      ]

  it "replaces an array with an array prism" $ do
    runSetTest "_Array" "[9]" "[1,2]"
    `shouldReturn` Right [JSONBeginArray, JSONNumber 9, JSONEndArray]

--------------------------------------------------------------------------------
-- delete
--------------------------------------------------------------------------------

deleteSpec :: Spec
deleteSpec = describe "delete" $ do
  it "removes a field" $ do
    runDeleteTest "#name" "{\"name\":\"alice\",\"age\":30}"
    `shouldReturn` Right
      [JSONBeginObject, JSONObjectKey "age", JSONNumber 30, JSONEndObject]

  it "leaves the document unchanged when the field is missing" $ do
    runDeleteTest "#name" "{\"age\":30}"
    `shouldReturn` Right
      [JSONBeginObject, JSONObjectKey "age", JSONNumber 30, JSONEndObject]

  it "leaves non-object input unchanged" $ do
    runDeleteTest "#name" "[1,2]"
    `shouldReturn` Right [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONEndArray]

  it "removes every array element" $ do
    runDeleteTest "each" "[1,2,3]"
    `shouldReturn` Right [JSONBeginArray, JSONEndArray]

  it "removes every object member" $ do
    runDeleteTest "each" "{\"a\":1,\"b\":2}"
    `shouldReturn` Right [JSONBeginObject, JSONEndObject]

  it "leaves an empty array unchanged" $ do
    runDeleteTest "each" "[]" `shouldReturn` Right [JSONBeginArray, JSONEndArray]

  it "leaves an empty object unchanged" $ do
    runDeleteTest "each" "{}"
    `shouldReturn` Right [JSONBeginObject, JSONEndObject]

  it "removes only strings with a prism" $ do
    runDeleteTest "each . _String" "[1,\"a\",2]"
    `shouldReturn` Right
      [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONEndArray]

  it "removes object members whose whole value matches a prism" $ do
    runDeleteTest "each . _Number" "{\"a\":1,\"b\":\"x\"}"
    `shouldReturn` Right
      [JSONBeginObject, JSONObjectKey "b", JSONString "x", JSONEndObject]

  it "removes the first array element" $ do
    runDeleteTest "_1" "[10,20]"
    `shouldReturn` Right [JSONBeginArray, JSONNumber 20, JSONEndArray]

  it "removes a member whose value is targeted by a composed optic" $ do
    runDeleteTest "#a._String" "{\"a\":\"x\",\"b\":\"y\"}"
    `shouldReturn` Right
      [JSONBeginObject, JSONObjectKey "b", JSONString "y", JSONEndObject]

  it "keeps a member whose value does not match the composed prism" $ do
    runDeleteTest "#a._String" "{\"a\":5,\"b\":\"y\"}"
    `shouldReturn` Right
      [ JSONBeginObject,
        JSONObjectKey "a",
        JSONNumber 5,
        JSONObjectKey "b",
        JSONString "y",
        JSONEndObject
      ]

  it "removes composed fields but keeps the container" $ do
    runDeleteTest "#users.each.#name" "{\"users\":[{\"name\":\"a\",\"age\":1},{\"age\":2}]}"
    `shouldReturn` Right
      [ JSONBeginObject,
        JSONObjectKey "users",
        JSONBeginArray,
        JSONBeginObject,
        JSONObjectKey "age",
        JSONNumber 1,
        JSONEndObject,
        JSONBeginObject,
        JSONObjectKey "age",
        JSONNumber 2,
        JSONEndObject,
        JSONEndArray,
        JSONEndObject
      ]

  it "removes the whole document with id" $ do
    runDeleteTest "id" "[1,2]" `shouldReturn` Right []

  it "leaves a value alone when the composed optic does not reach it" $ do
    runDeleteTest "#a.#b" "{\"a\":{\"c\":1},\"b\":2}"
    `shouldReturn` Right
      [ JSONBeginObject,
        JSONObjectKey "a",
        JSONBeginObject,
        JSONObjectKey "c",
        JSONNumber 1,
        JSONEndObject,
        JSONObjectKey "b",
        JSONNumber 2,
        JSONEndObject
      ]

-------------------------------------------------------------------------------
-- over
-------------------------------------------------------------------------------

overSpec :: Spec
overSpec = describe "over" $ do
  it "adds to every array element" $ do
    runOverTest "each" (add 1) "[1,2,3]"
    `shouldReturn` Right
      [JSONBeginArray, JSONNumber 2, JSONNumber 3, JSONNumber 4, JSONEndArray]

  it "adds to a field value" $ do
    runOverTest "#age" (add 1) "{\"name\":\"alice\",\"age\":30}"
    `shouldReturn` Right
      [ JSONBeginObject,
        JSONObjectKey "name",
        JSONString "alice",
        JSONObjectKey "age",
        JSONNumber 31,
        JSONEndObject
      ]

  it "appends to string values" $ do
    runOverTest "each . _String" (concatString "!") "[1,\"a\",\"b\"]"
    `shouldReturn` Right
      [ JSONBeginArray,
        JSONNumber 1,
        JSONString "a!",
        JSONString "b!",
        JSONEndArray
      ]

  it "trims string values" $ do
    runOverTest "each . _String" trim "[\"  hi  \",5]"
    `shouldReturn` Right
      [JSONBeginArray, JSONString "hi", JSONNumber 5, JSONEndArray]

  it "replaces substrings in string values" $ do
    runOverTest "#name" (replace "a" "e") "{\"name\":\"alice\"}"
    `shouldReturn` Right
      [JSONBeginObject, JSONObjectKey "name", JSONString "elice", JSONEndObject]

  it "maps values to booleans" $ do
    runOverTest "each" (equal (Number 1)) "[1,2,1]"
    `shouldReturn` Right
      [ JSONBeginArray,
        JSONBool True,
        JSONBool False,
        JSONBool True,
        JSONEndArray
      ]

  it "negates boolean values" $ do
    runOverTest "each . _Bool" not "[true,false]"
    `shouldReturn` Right
      [JSONBeginArray, JSONBool False, JSONBool True, JSONEndArray]

  it "disjoins two transformations with or" $ do
    runOverTest "each" (or (equal (Number 1)) (equal (Number 3))) "[1,2,3]"
    `shouldReturn` Right
      [ JSONBeginArray,
        JSONBool True,
        JSONBool False,
        JSONBool True,
        JSONEndArray
      ]

  it "composes transformations right-to-left" $ do
    runOverTest "each" (combine (equal (Number 3)) (add 1)) "[1,2,3]"
    `shouldReturn` Right
      [ JSONBeginArray,
        JSONBool False,
        JSONBool True,
        JSONBool False,
        JSONEndArray
      ]

  it "rewrites values focused by a composed optic" $ do
    runOverTest "#users.each.#age" (add 1) "{\"users\":[{\"age\":1},{\"age\":2}]}"
    `shouldReturn` Right
      [ JSONBeginObject,
        JSONObjectKey "users",
        JSONBeginArray,
        JSONBeginObject,
        JSONObjectKey "age",
        JSONNumber 2,
        JSONEndObject,
        JSONBeginObject,
        JSONObjectKey "age",
        JSONNumber 3,
        JSONEndObject,
        JSONEndArray,
        JSONEndObject
      ]

  it "replaces the whole document with id" $ do
    runOverTest "id" (equal (Number 1)) "[1,2]"
    `shouldReturn` Right [JSONBool False]

  it "fails when the transformation does not fit the value" $ do
    runOverTest "each" (add 1) "[\"a\",1]"
    `shouldReturn` Left "expected a number"

spec :: Spec
spec = describe "HQ.Runner" $ do
  eachArraySpec
  eachObjectSpec
  eachCompositionSpec
  fieldSpec
  idSpec
  previewSpec
  setSpec
  deleteSpec
  overSpec
  chunkedSpec

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

--------------------------------------------------------------------------------
-- chunked input: cursor behaviour across chunk boundaries
--------------------------------------------------------------------------------

chunkedSpec :: Spec
chunkedSpec = describe "chunked input" $ do
  it "folds agree with whole-input runs under every split" $ do
    forM_ foldChunkCases $ \(opticStr, doc) ->
      case parseOptic opticStr of
        Left err -> expectationFailure $ "bad optic: " <> show err
        Right optic -> do
          expected <- runFoldTest optic doc
          forM_ (chunkSplits doc) $ \chunks -> do
            actual <- runFoldChunks optic chunks
            actual `shouldBe` expected

  it "rewrites agree with whole-input runs under every split" $ do
    forM_ overChunkCases $ \(opticStr, transformation, doc) -> do
      expected <- runOverTest opticStr transformation doc
      case parseOptic opticStr of
        Left err -> expectationFailure $ "bad optic: " <> show err
        Right optic ->
          forM_ (chunkSplits doc) $ \chunks -> do
            actual <- runRewriteChunks (runOver optic transformation testConfig) chunks
            actual `shouldBe` expected

  it "deletes agree with whole-input runs under every split" $ do
    forM_ deleteChunkCases $ \(opticStr, doc) ->
      case parseOptic opticStr of
        Left err -> expectationFailure $ "bad optic: " <> show err
        Right optic -> do
          expected <- runDeleteTest opticStr doc
          forM_ (chunkSplits doc) $ \chunks -> do
            actual <- runRewriteChunks (runDelete optic testConfig) chunks
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

overChunkCases :: [(Text, Transformation, Text)]
overChunkCases =
  [ ("#users.each.#age", add 1, "{\"users\":[{\"age\":1},{\"age\":2}]}"),
    ("each", add 1, "[1,2,3]"),
    ("each . _String", concatString "!", "[1,\"a\",\"b\"]")
  ]

deleteChunkCases :: [(Text, Text)]
deleteChunkCases =
  [ ("#users.each.#name", "{\"users\":[{\"name\":\"a\",\"age\":1},{\"age\":2}]}"),
    ("each", "[1,2,3]"),
    ("#a", "{\"a\":{\"b\":[1,2]},\"c\":3}")
  ]

malformedChunkCases :: [(Text, Text)]
malformedChunkCases =
  [ ("#b", "{\"a\":1,\"b\":tru}"),
    ("#a", "{\"a\":1,\"b\":tru}"),
    ("each", "[1,,2]"),
    ("#a", "{\"a\":01}")
  ]
