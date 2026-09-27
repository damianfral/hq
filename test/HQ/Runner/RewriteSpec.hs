{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner.RewriteSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Text as T
import HQ.Error (HQError (..), renderHQError)
import HQ.JSON.Decoder (StreamIO, initialDecoder)
import HQ.JSON.Encoder (EncodeStyle (..), EncoderConfig (..), Join (..), Raw (..), ValueOptions (..), encodeChunks)
import HQ.JSON.Event (JSONEvent (..), eventsToValue)
import HQ.JSON.Parser (parseValueEvents)
import HQ.Optic.Parser (parseOptic)
import HQ.Runner.Cursor (Cursor (..), RewriteContinuation)
import HQ.Runner.Rewrite (runDelete, runOver)
import HQ.Transformation (Transformation, add, combine, concatString, constValue, equal, not, or, replace, trim)
import Relude hiding (Compose, id, many, not, or, some, subtract, toStrict)
import Streaming (Of (..))
import qualified Streaming.Prelude as S
import Test.HQ (chunkSplits)
import Test.Syd

spec :: Spec
spec = describe "HQ.Runner.Rewrite" $ do
  setSpec
  deleteSpec
  overSpec
  keysRewriteSpec
  valuesRewriteSpec
  ixRewriteSpec
  filterRewriteSpec
  chunkedRewriteSpec

-- | Encoder config for rewrite tests: pretty output, matching the
-- production default.
testConfig :: EncoderConfig
testConfig = EncoderConfig (Pretty 2) (ValueOptions NoRaw NoJoin)

-- | Run a document-rewriting query (set/delete), reparse its output
-- bytes back to events, and collect them. Reparsing through the
-- independent pure parser keeps every existing event expectation
-- valid while the rewrite pipeline emits chunks.
runRewriteTest :: RewriteContinuation -> Text -> IO (Either Text [JSONEvent])
runRewriteTest run input = runRewriteChunks run [input]

-- | Run a document-rewriting query against chunked JSON text.
runRewriteChunks :: RewriteContinuation -> [Text] -> IO (Either Text [JSONEvent])
runRewriteChunks run chunks = do
  result <- runExceptT $ do
    (outChunks :> _) <- S.toList (run cursor [])
    (byteChunks :> _) <- S.toList (encodeChunks 65536 (S.each outChunks))
    pure (decodeUtf8 (mconcat byteChunks))
  case first renderHQError result of
    Left err -> pure (Left err)
    Right text
      -- No output bytes means no values (e.g. deleting the whole
      -- document): nothing to reparse.
      | T.null (T.strip text) -> pure (Right [])
      | otherwise -> case parseValueEvents text of
          Left err -> pure (Left err)
          Right events -> pure (Right events)
  where
    textStream :: StreamIO Text ()
    textStream = S.each chunks
    cursor = Cursor [] initialDecoder textStream

-- | Parse an optic string and a replacement value, run @set@ against
-- JSON input, and collect the rewritten document's events.
--
-- @set@ is @over@ with a constant transformation, so the harness drives
-- 'runOver' with 'constValue'.
runSetTest :: Text -> Text -> Text -> IO (Either Text [JSONEvent])
runSetTest opticStr valueStr jsonInput = case parseOptic opticStr of
  Left err -> pure (Left (show err))
  Right optic -> case parseValueEvents valueStr of
    Left err -> pure (Left err)
    Right events -> case eventsToValue events of
      Left err -> pure (Left (renderHQError (HQRunnerError err)))
      Right value ->
        runRewriteTest (runOver optic (constValue value) testConfig) jsonInput

-- | Parse an optic string and run @delete@ against JSON input,
-- collecting the rewritten document's events.
runDeleteTest :: Text -> Text -> IO (Either Text [JSONEvent])
runDeleteTest opticStr jsonInput = case parseOptic opticStr of
  Left err -> pure (Left (show err))
  Right optic -> runRewriteTest (runDelete optic testConfig) jsonInput

-- | Parse an optic string and a transformation, run @over@ against JSON
-- input, and collect the rewritten document's events.
runOverTest :: Text -> Transformation -> Text -> IO (Either Text [JSONEvent])
runOverTest opticStr transformation jsonInput = case parseOptic opticStr of
  Left err -> pure (Left (show err))
  Right optic ->
    runRewriteTest (runOver optic transformation testConfig) jsonInput

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
    runSetTest "ix 0" "9" "[[1,2],[3]]"
    `shouldReturn` Right
      [ JSONBeginArray,
        JSONNumber 9,
        JSONBeginArray,
        JSONNumber 3,
        JSONEndArray,
        JSONEndArray
      ]

  it "replaces the second array element" $ do
    runSetTest "ix 1" "9" "[1,[2,3]]"
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
    runDeleteTest "ix 0" "[10,20]"
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

-------------------------------------------------------------------------------
-- keys rewrite (object key renaming; array indices are read-only)
-------------------------------------------------------------------------------

keysRewriteSpec :: Spec
keysRewriteSpec = describe "keys rewrite" $ do
  it "renames every key" $ do
    runOverTest "keys" (concatString "!") "{\"a\":1,\"b\":2}"
    `shouldReturn` Right
      [ JSONBeginObject,
        JSONObjectKey "a!",
        JSONNumber 1,
        JSONObjectKey "b!",
        JSONNumber 2,
        JSONEndObject
      ]

  it "trims keys" $ do
    runOverTest "keys" trim "{\" a \":1}"
    `shouldReturn` Right
      [JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject]

  it "renames through a composed string prism" $ do
    runOverTest "keys . _String" (concatString "!") "{\"a\":1}"
    `shouldReturn` Right
      [JSONBeginObject, JSONObjectKey "a!", JSONNumber 1, JSONEndObject]

  it "leaves keys alone when the prism does not match" $ do
    runOverTest "keys . _Number" (add 1) "{\"a\":1}"
    `shouldReturn` Right
      [JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject]

  it "leaves arrays unchanged" $ do
    runOverTest "keys" (concatString "!") "[1,2]"
    `shouldReturn` Right [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONEndArray]

  it "sets every key to a constant" $ do
    runSetTest "keys" "\"k\"" "{\"a\":1,\"b\":2}"
    `shouldReturn` Right
      [ JSONBeginObject,
        JSONObjectKey "k",
        JSONNumber 1,
        JSONObjectKey "k",
        JSONNumber 2,
        JSONEndObject
      ]

  it "fails when the replacement is not a string" $ do
    runSetTest "keys" "5" "{\"a\":1}"
    `shouldReturn` Left "key transformation must yield a string"

  it "fails when the transformation does not fit keys" $ do
    runOverTest "keys" (add 1) "{\"a\":1}"
    `shouldReturn` Left "expected a number"

  it "removes every member" $ do
    runDeleteTest "keys" "{\"a\":1,\"b\":2}"
    `shouldReturn` Right [JSONBeginObject, JSONEndObject]

  it "removes every member through a matching prism" $ do
    runDeleteTest "keys . _String" "{\"a\":1}"
    `shouldReturn` Right [JSONBeginObject, JSONEndObject]

  it "keeps members when the prism does not match" $ do
    runDeleteTest "keys . _Number" "{\"a\":1}"
    `shouldReturn` Right
      [JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject]

  it "leaves arrays unchanged on delete" $ do
    runDeleteTest "keys" "[1,2]"
    `shouldReturn` Right [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONEndArray]

-------------------------------------------------------------------------------
-- values rewrite (objects only)
-------------------------------------------------------------------------------

valuesRewriteSpec :: Spec
valuesRewriteSpec = describe "values rewrite" $ do
  it "adds to every object value" $ do
    runOverTest "values" (add 1) "{\"a\":1,\"b\":2}"
    `shouldReturn` Right
      [ JSONBeginObject,
        JSONObjectKey "a",
        JSONNumber 2,
        JSONObjectKey "b",
        JSONNumber 3,
        JSONEndObject
      ]

  it "leaves arrays unchanged" $ do
    runOverTest "values" (add 1) "[1,2,3]"
    `shouldReturn` Right
      [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONNumber 3, JSONEndArray]

  it "rewrites values focused by a composed prism" $ do
    runOverTest "values . _Number" (add 1) "{\"a\":1,\"b\":\"x\"}"
    `shouldReturn` Right
      [ JSONBeginObject,
        JSONObjectKey "a",
        JSONNumber 2,
        JSONObjectKey "b",
        JSONString "x",
        JSONEndObject
      ]

  it "sets every object value" $ do
    runSetTest "values" "0" "{\"a\":1,\"b\":2}"
    `shouldReturn` Right
      [ JSONBeginObject,
        JSONObjectKey "a",
        JSONNumber 0,
        JSONObjectKey "b",
        JSONNumber 0,
        JSONEndObject
      ]

  it "removes every member" $ do
    runDeleteTest "values" "{\"a\":1,\"b\":2}"
    `shouldReturn` Right [JSONBeginObject, JSONEndObject]

  it "leaves arrays unchanged on delete" $ do
    runDeleteTest "values" "[1,2]"
    `shouldReturn` Right [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONEndArray]

-------------------------------------------------------------------------------
-- ix rewrite (arrays only)
-------------------------------------------------------------------------------

ixRewriteSpec :: Spec
ixRewriteSpec = describe "ix rewrite" $ do
  it "adds to the indexed element" $ do
    runOverTest "ix 1" (add 10) "[1,2,3]"
    `shouldReturn` Right
      [JSONBeginArray, JSONNumber 1, JSONNumber 12, JSONNumber 3, JSONEndArray]

  it "leaves objects unchanged" $ do
    runOverTest "ix 0" (add 1) "{\"a\":1}"
    `shouldReturn` Right
      [JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject]

  it "leaves objects unchanged on delete" $ do
    runDeleteTest "ix 0" "{\"a\":1}"
    `shouldReturn` Right
      [JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject]

-------------------------------------------------------------------------------
-- filter rewrite
-------------------------------------------------------------------------------

filterRewriteSpec :: Spec
filterRewriteSpec = describe "filter rewrite" $ do
  it "replaces kept values" $ do
    runOverTest "each . filter #age == 30" (constValue (Number 0)) "[{\"age\":30},{\"age\":20}]"
    `shouldReturn` Right
      [JSONBeginArray, JSONNumber 0, JSONBeginObject, JSONObjectKey "age", JSONNumber 20, JSONEndObject, JSONEndArray]

  it "rewrites through a filter into kept values" $ do
    runOverTest "each . filter #age == 30 . #score" (add 100) "[{\"age\":30,\"score\":1},{\"age\":20,\"score\":2}]"
    `shouldReturn` Right
      [ JSONBeginArray,
        JSONBeginObject,
        JSONObjectKey "age",
        JSONNumber 30,
        JSONObjectKey "score",
        JSONNumber 101,
        JSONEndObject,
        JSONBeginObject,
        JSONObjectKey "age",
        JSONNumber 20,
        JSONObjectKey "score",
        JSONNumber 2,
        JSONEndObject,
        JSONEndArray
      ]

  it "sets kept values" $ do
    runSetTest "each . filter #age == 30" "0" "[{\"age\":30},{\"age\":20}]"
    `shouldReturn` Right
      [JSONBeginArray, JSONNumber 0, JSONBeginObject, JSONObjectKey "age", JSONNumber 20, JSONEndObject, JSONEndArray]

  it "removes kept elements" $ do
    runDeleteTest "each . filter #age == 30" "[{\"age\":30},{\"age\":20}]"
    `shouldReturn` Right
      [JSONBeginArray, JSONBeginObject, JSONObjectKey "age", JSONNumber 20, JSONEndObject, JSONEndArray]

  it "removes kept members but keeps the container" $ do
    runDeleteTest "#users . each . filter #age == 30" "{\"users\":[{\"age\":30},{\"age\":20}],\"b\":2}"
    `shouldReturn` Right
      [ JSONBeginObject,
        JSONObjectKey "users",
        JSONBeginArray,
        JSONBeginObject,
        JSONObjectKey "age",
        JSONNumber 20,
        JSONEndObject,
        JSONEndArray,
        JSONObjectKey "b",
        JSONNumber 2,
        JSONEndObject
      ]

  it "removes the whole member when the gate keeps it" $ do
    runDeleteTest "#users . filter (each == 1)" "{\"users\":[1],\"b\":2}"
    `shouldReturn` Right
      [JSONBeginObject, JSONObjectKey "b", JSONNumber 2, JSONEndObject]

  it "removes the whole document when the gate keeps it" $ do
    runDeleteTest "filter #age == 30" "{\"age\":30}" `shouldReturn` Right []

  it "leaves the document unchanged when the gate drops it" $ do
    runDeleteTest "filter #age == 30" "{\"age\":20}"
    `shouldReturn` Right
      [JSONBeginObject, JSONObjectKey "age", JSONNumber 20, JSONEndObject]

  it "fails when the predicate does not fit" $ do
    runOverTest "each . filter #a +1" (constValue (Number 0)) "[{\"a\":\"x\"}]"
    `shouldReturn` Left "expected a number"

  it "fails when the predicate is not boolean" $ do
    runOverTest "each . filter #a +1" (constValue (Number 0)) "[{\"a\":1}]"
    `shouldReturn` Left "filter transformation must produce a boolean"

chunkedRewriteSpec :: Spec
chunkedRewriteSpec = describe "chunked input" $ do
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

overChunkCases :: [(Text, Transformation, Text)]
overChunkCases =
  [ ("#users.each.#age", add 1, "{\"users\":[{\"age\":1},{\"age\":2}]}"),
    ("each", add 1, "[1,2,3]"),
    ("each . _String", concatString "!", "[1,\"a\",\"b\"]"),
    ("keys", concatString "!", "{\"a\":1,\"b\":2}"),
    ("values", add 1, "{\"a\":1,\"b\":2}"),
    ("ix 1", add 1, "[1,2,3]"),
    ("each . filter #age == 30", constValue (Number 0), "[{\"age\":30},{\"age\":20}]")
  ]

deleteChunkCases :: [(Text, Text)]
deleteChunkCases =
  [ ("#users.each.#name", "{\"users\":[{\"name\":\"a\",\"age\":1},{\"age\":2}]}"),
    ("each", "[1,2,3]"),
    ("#a", "{\"a\":{\"b\":[1,2]},\"c\":3}"),
    ("keys", "{\"a\":1,\"b\":2}"),
    ("values", "{\"a\":1,\"b\":2}"),
    ("each . filter #age == 30", "[{\"age\":30},{\"age\":20}]")
  ]
