{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner.RewriteSpec (spec) where

import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import HQ.Early (Early, runEarly)
import HQ.Error (HQError (..), renderHQError)
import HQ.JSON.Decoder (StreamIO, initialDecoder)
import HQ.JSON.Encoder
import HQ.JSON.Event (JSONEvent (..), eventsToValue)
import HQ.JSON.Parser (parseValueEvents)
import HQ.Optic (Optic)
import HQ.Optic.Parser (parseOptic)
import HQ.Runner (rewriteDocuments)
import HQ.Runner.Cursor (Cursor (..))
import HQ.Runner.Rewrite (runDelete, runOver)
import HQ.Runner.Rewrite.Emit (RewriteContinuation)
import HQ.Transformation
import Relude hiding (Compose, Const, and, id, length, many, not, or, reverse, some, subtract, toStrict)
import Streaming (Of (..))
import Streaming.Prelude qualified as S
import Test.HQ (splits)
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
  compactContainerSpec
  multiDocumentRewriteSpec

-- | Encoder config for rewrite tests: pretty output, matching the
-- production default.
testConfig :: EncoderConfig
testConfig = EncoderConfig (Pretty 2) (ValueOptions NoRaw NoJoin)

-- | Encoder config exercising the compact fast paths (raw scalar and
-- container capture) in Take. Same value options as 'testConfig',
-- only the style differs.
compactTestConfig :: EncoderConfig
compactTestConfig = EncoderConfig Compact (ValueOptions NoRaw NoJoin)

-- | Whole-container passthrough values: nested objects/arrays with
-- strings (escapes, unicode), numbers, keywords, empties and
-- significant whitespace (dropped by compact capture).
containerCorpus :: [Text]
containerCorpus =
  [ "{\"a\":{\"b\":[1,\"x\",true,null]},\"c\":{}}",
    "{\"a\":[{\"b\":[{\"c\":[]}]}]}",
    "[{\"k\":\"v\\\"q\",\"n\":-12.5e2,\"t\":true,\"f\":false,\"z\":null,\"e\":{},\"s\":\"a\\u00e9\\n\"}]",
    "{ \"a\" : [1, 2] }",
    "{\"deep\":{\"deeper\":{\"deepest\":[[[[1]]]]}}}",
    "[[],{},[{}],{\"a\":[]}]"
  ]

malformedContainerCorpus :: [Text]
malformedContainerCorpus =
  [ "{\"a\":[1,2}",
    "{\"a\":1",
    "[1,,2]",
    "[1 2]",
    "{\"a\" 1}",
    "{\"a\":\"\\x\"}",
    "{\"a\":nul}",
    "{\"a\":01}"
  ]

-- | Compact container capture must agree with the event path
-- (pretty config): same queries, same documents, every chunk split.
-- The pretty runs decode and re-encode event by event; the compact
-- runs capture whole containers verbatim.
compactContainerSpec :: Spec
compactContainerSpec = describe "compact container capture" $ do
  it "over agrees with the event path under every split" $ do
    forM_ overContainerCases $ \(opticStr, transformation, doc) ->
      case parseOptic opticStr of
        Left err -> expectationFailure $ "bad optic: " <> show err
        Right optic -> do
          expected <- runRewriteChunks (\early -> runOver early optic transformation testConfig) [doc]
          forM_ (splits doc) $ \chunks -> do
            actual <- runRewriteChunks (\early -> runOver early optic transformation compactTestConfig) chunks
            actual `shouldBe` expected
  it "delete agrees with the event path under every split" $ do
    forM_ deleteContainerCases $ \(opticStr, doc) ->
      case parseOptic opticStr of
        Left err -> expectationFailure $ "bad optic: " <> show err
        Right optic -> do
          expected <- runRewriteChunks (\early -> runDelete early optic testConfig) [doc]
          forM_ (splits doc) $ \chunks -> do
            actual <- runRewriteChunks (\early -> runDelete early optic compactTestConfig) chunks
            actual `shouldBe` expected
  it "fails exactly where the event path fails" $ do
    Right testOptic <- pure (parseOptic "@a")
    forM_ malformedContainerCorpus $ \doc -> do
      forM_ (splits doc) $ \chunks -> do
        pretty <- runRewriteChunks (\early -> runDelete early testOptic testConfig) chunks
        compact <- runRewriteChunks (\early -> runDelete early testOptic compactTestConfig) chunks
        pretty `shouldSatisfy` isLeft
        compact `shouldBe` pretty
  it "normalizes insignificant whitespace like the event path" $ do
    Right optic <- pure (parseOptic "@b")
    runRewriteDocumentsTest (\early -> runDelete early optic compactTestConfig) ["{ \"a\" : [1, 2] }"]
      `shouldReturn` Right "{\"a\":[1,2]}\n"
  where
    overContainerCases :: [(Text, Transformation, Text)]
    overContainerCases =
      [ ("@a", Add 1, "{\"a\":1,\"nested\":{\"x\":[1,2],\"y\":{}}}"),
        ("@a", Add 1, "{ \"a\" : 1, \"spaced\" : [1, 2] }"),
        ("each.@n", Add 1, "[{\"n\":1,\"keep\":{\"deep\":[true,null]}},{\"n\":2}]")
      ]
    deleteContainerCases :: [(Text, Text)]
    deleteContainerCases =
      [ ("@x", "{\"x\":1,\"keep\":{\"deep\":[1,{\"y\":2}]}}"),
        ("@a", "{\"a\":1,\"nested\":{\"x\":[1,2],\"y\":{}}}")
      ]
        <> [("@x", doc) | doc <- containerCorpus]

-- | Run a document-rewriting query (set/delete), reparse its output
-- bytes back to events, and collect them. Reparsing through the
-- independent pure parser keeps every existing event expectation
-- valid while the rewrite pipeline emits chunks.
runRewriteTest :: (Early HQError -> RewriteContinuation) -> Text -> IO (Either Text [JSONEvent])
runRewriteTest mkRun input = runRewriteChunks mkRun [input]

-- | Run a document-rewriting query against chunked JSON text.
runRewriteChunks :: (Early HQError -> RewriteContinuation) -> [Text] -> IO (Either Text [JSONEvent])
runRewriteChunks mkRun chunks = do
  result <-
    runEarly
      ( \early -> do
          (byteChunks :> _) <- S.toList (mkRun early cursor initialEmitState)
          pure (decodeUtf8 (mconcat byteChunks))
      )
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

-- | Parse an optic string and run the continuation, mapping optic
-- syntax errors like any other failure.
withOptic :: Text -> (Optic -> IO (Either Text [JSONEvent])) -> IO (Either Text [JSONEvent])
withOptic opticStr f = case parseOptic opticStr of
  Left err -> pure (Left (show err))
  Right optic -> f optic

-- | Parse an optic string and a replacement value, run @set@ against
-- JSON input, and collect the rewritten document's events.
--
-- @set@ is @over@ with a constant transformation, so the harness drives
-- 'runOver' with 'Const'.
runSetTest :: Text -> Text -> Text -> IO (Either Text [JSONEvent])
runSetTest opticStr valueStr jsonInput = withOptic opticStr $ \optic ->
  case parseValueEvents valueStr of
    Left err -> pure (Left err)
    Right events -> case eventsToValue events of
      Left err -> pure (Left (renderHQError (HQRunnerError err)))
      Right value ->
        runRewriteTest (\early -> runOver early optic (Const value) testConfig) jsonInput

-- | Parse an optic string and run @delete@ against JSON input,
-- collecting the rewritten document's events.
runDeleteTest :: Text -> Text -> IO (Either Text [JSONEvent])
runDeleteTest opticStr jsonInput = withOptic opticStr $ \optic ->
  runRewriteTest (\early -> runDelete early optic testConfig) jsonInput

-- | Parse an optic string and a transformation, run @over@ against JSON
-- input, and collect the rewritten document's events.
runOverTest :: Text -> Transformation -> Text -> IO (Either Text [JSONEvent])
runOverTest opticStr transformation jsonInput = withOptic opticStr $ \optic ->
  runRewriteTest (\early -> runOver early optic transformation testConfig) jsonInput

--------------------------------------------------------------------------------
-- set
--------------------------------------------------------------------------------

setSpec :: Spec
setSpec = describe "set" $ forM_ setCases $ \(label, optic, value, doc, expected) ->
  it label $ runSetTest optic value doc `shouldReturn` expected

setCases :: [(String, Text, Text, Text, Either Text [JSONEvent])]
setCases =
  [ ("replaces a field value", "@name", "\"bob\"", "{\"name\":\"alice\",\"age\":30}", Right [JSONBeginObject, JSONObjectKey "name", JSONString "bob", JSONObjectKey "age", JSONNumber 30, JSONEndObject]),
    ("leaves the document unchanged when the field is missing", "@name", "5", "{\"age\":30}", Right [JSONBeginObject, JSONObjectKey "age", JSONNumber 30, JSONEndObject]),
    ("leaves non-object input unchanged", "@name", "5", "[1,2]", Right [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONEndArray]),
    ("replaces every array element", "each", "0", "[1,2,3]", Right [JSONBeginArray, JSONNumber 0, JSONNumber 0, JSONNumber 0, JSONEndArray]),
    ("replaces every object value", "each", "true", "{\"a\":1,\"b\":2}", Right [JSONBeginObject, JSONObjectKey "a", JSONBool True, JSONObjectKey "b", JSONBool True, JSONEndObject]),
    ("leaves an empty array unchanged", "each", "0", "[]", Right [JSONBeginArray, JSONEndArray]),
    ("leaves an empty object unchanged", "each", "0", "{}", Right [JSONBeginObject, JSONEndObject]),
    ("replaces only strings with a prism", "each . _String", "\"x\"", "[1,\"a\",true]", Right [JSONBeginArray, JSONNumber 1, JSONString "x", JSONBool True, JSONEndArray]),
    ("passes non-matching values through unchanged", "_String", "\"x\"", "5", Right [JSONNumber 5]),
    ("replaces fields in a composed traversal", "@users.each.@name", "\"anon\"", "{\"users\":[{\"name\":\"a\",\"age\":1},{\"name\":\"b\"}]}", Right [JSONBeginObject, JSONObjectKey "users", JSONBeginArray, JSONBeginObject, JSONObjectKey "name", JSONString "anon", JSONObjectKey "age", JSONNumber 1, JSONEndObject, JSONBeginObject, JSONObjectKey "name", JSONString "anon", JSONEndObject, JSONEndArray, JSONEndObject]),
    ("keeps members unchanged when the composed target is missing", "@users.each.@name", "\"anon\"", "{\"users\":[{\"age\":1}]}", Right [JSONBeginObject, JSONObjectKey "users", JSONBeginArray, JSONBeginObject, JSONObjectKey "age", JSONNumber 1, JSONEndObject, JSONEndArray, JSONEndObject]),
    ("replaces the first array element", "ix 0", "9", "[[1,2],[3]]", Right [JSONBeginArray, JSONNumber 9, JSONBeginArray, JSONNumber 3, JSONEndArray, JSONEndArray]),
    ("replaces the second array element", "ix 1", "9", "[1,[2,3]]", Right [JSONBeginArray, JSONNumber 1, JSONNumber 9, JSONEndArray]),
    ("replaces the whole document with id", "id", "5", "[1,2]", Right [JSONNumber 5]),
    ("replaces the whole document with a compound value", "id", "{\"x\":1}", "5", Right [JSONBeginObject, JSONObjectKey "x", JSONNumber 1, JSONEndObject]),
    ("replaces a nested container value wholesale", "@a", "5", "{\"a\":{\"b\":[1,2]},\"c\":3}", Right [JSONBeginObject, JSONObjectKey "a", JSONNumber 5, JSONObjectKey "c", JSONNumber 3, JSONEndObject]),
    ("replaces an array with an array prism", "_Array", "[9]", "[1,2]", Right [JSONBeginArray, JSONNumber 9, JSONEndArray])
  ]

--------------------------------------------------------------------------------
-- delete
--------------------------------------------------------------------------------

deleteSpec :: Spec
deleteSpec = describe "delete" $ forM_ deleteCases $ \(label, optic, doc, expected) ->
  it label $ runDeleteTest optic doc `shouldReturn` expected

deleteCases :: [(String, Text, Text, Either Text [JSONEvent])]
deleteCases =
  [ ("removes a field", "@name", "{\"name\":\"alice\",\"age\":30}", Right [JSONBeginObject, JSONObjectKey "age", JSONNumber 30, JSONEndObject]),
    ("leaves the document unchanged when the field is missing", "@name", "{\"age\":30}", Right [JSONBeginObject, JSONObjectKey "age", JSONNumber 30, JSONEndObject]),
    ("leaves non-object input unchanged", "@name", "[1,2]", Right [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONEndArray]),
    ("removes every array element", "each", "[1,2,3]", Right [JSONBeginArray, JSONEndArray]),
    ("removes every object member", "each", "{\"a\":1,\"b\":2}", Right [JSONBeginObject, JSONEndObject]),
    ("leaves an empty array unchanged", "each", "[]", Right [JSONBeginArray, JSONEndArray]),
    ("leaves an empty object unchanged", "each", "{}", Right [JSONBeginObject, JSONEndObject]),
    ("removes only strings with a prism", "each . _String", "[1,\"a\",2]", Right [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONEndArray]),
    ("removes object members whose whole value matches a prism", "each . _Number", "{\"a\":1,\"b\":\"x\"}", Right [JSONBeginObject, JSONObjectKey "b", JSONString "x", JSONEndObject]),
    ("removes the first array element", "ix 0", "[10,20]", Right [JSONBeginArray, JSONNumber 20, JSONEndArray]),
    ("removes a member whose value is targeted by a composed optic", "@a._String", "{\"a\":\"x\",\"b\":\"y\"}", Right [JSONBeginObject, JSONObjectKey "b", JSONString "y", JSONEndObject]),
    ("keeps a member whose value does not match the composed prism", "@a._String", "{\"a\":5,\"b\":\"y\"}", Right [JSONBeginObject, JSONObjectKey "a", JSONNumber 5, JSONObjectKey "b", JSONString "y", JSONEndObject]),
    ("removes composed fields but keeps the container", "@users.each.@name", "{\"users\":[{\"name\":\"a\",\"age\":1},{\"age\":2}]}", Right [JSONBeginObject, JSONObjectKey "users", JSONBeginArray, JSONBeginObject, JSONObjectKey "age", JSONNumber 1, JSONEndObject, JSONBeginObject, JSONObjectKey "age", JSONNumber 2, JSONEndObject, JSONEndArray, JSONEndObject]),
    ("removes the whole document with id", "id", "[1,2]", Right []),
    ("leaves a value alone when the composed optic does not reach it", "@a.@b", "{\"a\":{\"c\":1},\"b\":2}", Right [JSONBeginObject, JSONObjectKey "a", JSONBeginObject, JSONObjectKey "c", JSONNumber 1, JSONEndObject, JSONObjectKey "b", JSONNumber 2, JSONEndObject])
  ]

-------------------------------------------------------------------------------
-- over
-------------------------------------------------------------------------------

overSpec :: Spec
overSpec = describe "over" $ forM_ overCases $ \(label, optic, t, doc, expected) ->
  it label $ runOverTest optic t doc `shouldReturn` expected

overCases :: [(String, Text, Transformation, Text, Either Text [JSONEvent])]
overCases =
  [ ("adds to every array element", "each", Add 1, "[1,2,3]", Right [JSONBeginArray, JSONNumber 2, JSONNumber 3, JSONNumber 4, JSONEndArray]),
    ("adds to a field value", "@age", Add 1, "{\"name\":\"alice\",\"age\":30}", Right [JSONBeginObject, JSONObjectKey "name", JSONString "alice", JSONObjectKey "age", JSONNumber 31, JSONEndObject]),
    ("appends to string values", "each . _String", ConcatString "!", "[1,\"a\",\"b\"]", Right [JSONBeginArray, JSONNumber 1, JSONString "a!", JSONString "b!", JSONEndArray]),
    ("trims string values", "each . _String", Trim, "[\"  hi  \",5]", Right [JSONBeginArray, JSONString "hi", JSONNumber 5, JSONEndArray]),
    ("replaces substrings in string values", "@name", Replace "a" "e", "{\"name\":\"alice\"}", Right [JSONBeginObject, JSONObjectKey "name", JSONString "elice", JSONEndObject]),
    ("maps values to booleans", "each", Equal (Number 1), "[1,2,1]", Right [JSONBeginArray, JSONBool True, JSONBool False, JSONBool True, JSONEndArray]),
    ("negates boolean values", "each . _Bool", Not, "[true,false]", Right [JSONBeginArray, JSONBool False, JSONBool True, JSONEndArray]),
    ("disjoins two transformations with or", "each", Or (Equal (Number 1)) (Equal (Number 3)), "[1,2,3]", Right [JSONBeginArray, JSONBool True, JSONBool False, JSONBool True, JSONEndArray]),
    ("conjoins two transformations with and", "each", And (Equal (Number 1)) (Equal (Number 1)), "[1,2]", Right [JSONBeginArray, JSONBool True, JSONBool False, JSONEndArray]),
    ("conjoins to true when both branches hold", "each", And (IsPrefixOf "a") (IsSuffixOf "c"), "[\"abc\",\"ab\"]", Right [JSONBeginArray, JSONBool True, JSONBool False, JSONEndArray]),
    -- The right branch would fail on a number; a false left branch
    -- must return False without running it.
    ("short-circuits and on false", "each", And (Equal (Number 2)) Not, "[1]", Right [JSONBeginArray, JSONBool False, JSONEndArray]),
    ("exclusive-disjoins two transformations with Xor", "each", Xor (IsPrefixOf "a") (IsSuffixOf "a"), "[\"a\",\"ab\",\"ba\",\"b\"]", Right [JSONBeginArray, JSONBool False, JSONBool True, JSONBool True, JSONBool False, JSONEndArray]),
    ("fails and with a non-boolean left branch", "each", And (Add 1) (Equal (Number 1)), "[0]", Left "expected a boolean result from the left side of and"),
    ("fails Xor with a non-boolean branch", "each", Xor (Equal (Number 1)) (Add 1), "[1]", Left "expected a boolean result from each side of xor"),
    ("composes transformations right-to-left", "each", Compose (Equal (Number 3)) (Add 1), "[1,2,3]", Right [JSONBeginArray, JSONBool False, JSONBool True, JSONBool False, JSONEndArray]),
    ("rewrites values focused by a composed optic", "@users.each.@age", Add 1, "{\"users\":[{\"age\":1},{\"age\":2}]}", Right [JSONBeginObject, JSONObjectKey "users", JSONBeginArray, JSONBeginObject, JSONObjectKey "age", JSONNumber 2, JSONEndObject, JSONBeginObject, JSONObjectKey "age", JSONNumber 3, JSONEndObject, JSONEndArray, JSONEndObject]),
    ("replaces the whole document with id", "id", Equal (Number 1), "[1,2]", Right [JSONBool False]),
    ("fails when the transformation does not fit the value", "each", Add 1, "[\"a\",1]", Left "expected a number"),
    ("strips a matching prefix", "each", StripPrefix "pre", "[\"prefix\",\"pre\",\"other\"]", Right [JSONBeginArray, JSONString "fix", JSONString "", JSONString "other", JSONEndArray]),
    ("strips a matching suffix", "each", StripSuffix ".json", "[\"a.json\",\"json\"]", Right [JSONBeginArray, JSONString "a", JSONString "json", JSONEndArray]),
    ("tests prefix membership", "each", IsPrefixOf "pre", "[\"prefix\",\"other\"]", Right [JSONBeginArray, JSONBool True, JSONBool False, JSONEndArray]),
    ("tests suffix membership", "each", IsSuffixOf ".json", "[\"a.json\",\"other\"]", Right [JSONBeginArray, JSONBool True, JSONBool False, JSONEndArray]),
    ("tests infix membership", "each", IsInfixOf "fix", "[\"prefix\",\"other\"]", Right [JSONBeginArray, JSONBool True, JSONBool False, JSONEndArray]),
    ("tests array emptiness", "each", IsEmpty, "[[],[1]]", Right [JSONBeginArray, JSONBool True, JSONBool False, JSONEndArray]),
    ("computes array lengths", "each", ArrayLength, "[[1,2],[]]", Right [JSONBeginArray, JSONNumber 2, JSONNumber 0, JSONEndArray]),
    ("reverses arrays", "each", ArrayReverse, "[[1,2,3]]", Right [JSONBeginArray, JSONBeginArray, JSONNumber 3, JSONNumber 2, JSONNumber 1, JSONEndArray, JSONEndArray]),
    ("drops duplicate array elements", "each", ArrayUnique, "[[1,1,2,1]]", Right [JSONBeginArray, JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONEndArray, JSONEndArray]),
    ("sorts array elements", "each", ArraySort, "[[3,1,2]]", Right [JSONBeginArray, JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONNumber 3, JSONEndArray, JSONEndArray]),
    ("fails sort on scalar elements", "each", ArraySort, "[\"b\",1,true,null]", Left "expected an array"),
    ("sorts mixed values by type rank", "each", ArraySort, "[[\"b\",1,true,null]]", Right [JSONBeginArray, JSONBeginArray, JSONNull, JSONBool True, JSONNumber 1, JSONString "b", JSONEndArray, JSONEndArray]),
    ("sorts nested arrays lexicographically", "each", ArraySort, "[[[2],[1],[1,2]]]", Right [JSONBeginArray, JSONBeginArray, JSONBeginArray, JSONNumber 1, JSONEndArray, JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONEndArray, JSONBeginArray, JSONNumber 2, JSONEndArray, JSONEndArray, JSONEndArray]),
    ("fails sort on non-arrays", "each", ArraySort, "[1]", Left "expected an array"),
    ("shallow-merges objects with right-wins conflicts", "id", Merge (KeyMap.fromList [("a", Object (KeyMap.fromList [("y", Number 2)])), ("b", Number 5)]), "{\"a\":{\"x\":1},\"b\":1}", Right [JSONBeginObject, JSONObjectKey "a", JSONBeginObject, JSONObjectKey "y", JSONNumber 2, JSONEndObject, JSONObjectKey "b", JSONNumber 5, JSONEndObject]),
    ("deep-merges nested objects recursively", "id", DeepMerge (KeyMap.fromList [("a", Object (KeyMap.fromList [("y", Number 2)])), ("b", Number 5)]), "{\"a\":{\"x\":1},\"b\":1}", Right [JSONBeginObject, JSONObjectKey "a", JSONBeginObject, JSONObjectKey "x", JSONNumber 1, JSONObjectKey "y", JSONNumber 2, JSONEndObject, JSONObjectKey "b", JSONNumber 5, JSONEndObject]),
    ("replaces conflicting scalars on deepMerge", "id", DeepMerge (KeyMap.fromList [("a", Number 5)]), "{\"a\":{\"x\":1}}", Right [JSONBeginObject, JSONObjectKey "a", JSONNumber 5, JSONEndObject]),
    ("fails merge on non-objects", "id", Merge (KeyMap.fromList [("b", Number 1)]), "[1]", Left "expected an object"),
    ("fails stripPrefix on numbers", "each", StripPrefix "a", "[1]", Left "expected a string"),
    ("fails length on strings", "each", ArrayLength, "[\"a\"]", Left "expected an array")
  ]

-------------------------------------------------------------------------------
-- keys rewrite (object key renaming; array indices are read-only)
-------------------------------------------------------------------------------

keysRewriteSpec :: Spec
keysRewriteSpec = describe "keys rewrite" $ do
  forM_ keysOverCases $ \(label, optic, t, doc, expected) ->
    it label $ runOverTest optic t doc `shouldReturn` expected
  forM_ keysSetCases $ \(label, optic, value, doc, expected) ->
    it label $ runSetTest optic value doc `shouldReturn` expected
  forM_ keysDeleteCases $ \(label, optic, doc, expected) ->
    it label $ runDeleteTest optic doc `shouldReturn` expected

keysOverCases :: [(String, Text, Transformation, Text, Either Text [JSONEvent])]
keysOverCases =
  [ ("renames every key", "keys", ConcatString "!", "{\"a\":1,\"b\":2}", Right [JSONBeginObject, JSONObjectKey "a!", JSONNumber 1, JSONObjectKey "b!", JSONNumber 2, JSONEndObject]),
    ("trims keys", "keys", Trim, "{\" a \":1}", Right [JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject]),
    ("renames through a composed string prism", "keys . _String", ConcatString "!", "{\"a\":1}", Right [JSONBeginObject, JSONObjectKey "a!", JSONNumber 1, JSONEndObject]),
    ("leaves keys alone when the prism does not match", "keys . _Number", Add 1, "{\"a\":1}", Right [JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject]),
    ("leaves arrays unchanged", "keys", ConcatString "!", "[1,2]", Right [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONEndArray]),
    ("fails when the transformation does not fit keys", "keys", Add 1, "{\"a\":1}", Left "expected a number")
  ]

keysSetCases :: [(String, Text, Text, Text, Either Text [JSONEvent])]
keysSetCases =
  [ ("sets every key to a constant", "keys", "\"k\"", "{\"a\":1,\"b\":2}", Right [JSONBeginObject, JSONObjectKey "k", JSONNumber 1, JSONObjectKey "k", JSONNumber 2, JSONEndObject]),
    ("fails when the replacement is not a string", "keys", "5", "{\"a\":1}", Left "key transformation must yield a string")
  ]

keysDeleteCases :: [(String, Text, Text, Either Text [JSONEvent])]
keysDeleteCases =
  [ ("removes every member", "keys", "{\"a\":1,\"b\":2}", Right [JSONBeginObject, JSONEndObject]),
    ("removes every member through a matching prism", "keys . _String", "{\"a\":1}", Right [JSONBeginObject, JSONEndObject]),
    ("keeps members when the prism does not match", "keys . _Number", "{\"a\":1}", Right [JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject]),
    ("leaves arrays unchanged on delete", "keys", "[1,2]", Right [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONEndArray])
  ]

-------------------------------------------------------------------------------
-- values rewrite (objects only)
-------------------------------------------------------------------------------

valuesRewriteSpec :: Spec
valuesRewriteSpec = describe "values rewrite" $ do
  forM_ valuesOverCases $ \(label, optic, t, doc, expected) ->
    it label $ runOverTest optic t doc `shouldReturn` expected
  forM_ valuesSetCases $ \(label, optic, value, doc, expected) ->
    it label $ runSetTest optic value doc `shouldReturn` expected
  forM_ valuesDeleteCases $ \(label, optic, doc, expected) ->
    it label $ runDeleteTest optic doc `shouldReturn` expected

valuesOverCases :: [(String, Text, Transformation, Text, Either Text [JSONEvent])]
valuesOverCases =
  [ ("adds to every object value", "values", Add 1, "{\"a\":1,\"b\":2}", Right [JSONBeginObject, JSONObjectKey "a", JSONNumber 2, JSONObjectKey "b", JSONNumber 3, JSONEndObject]),
    ("leaves arrays unchanged", "values", Add 1, "[1,2,3]", Right [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONNumber 3, JSONEndArray]),
    ("rewrites values focused by a composed prism", "values . _Number", Add 1, "{\"a\":1,\"b\":\"x\"}", Right [JSONBeginObject, JSONObjectKey "a", JSONNumber 2, JSONObjectKey "b", JSONString "x", JSONEndObject])
  ]

valuesSetCases :: [(String, Text, Text, Text, Either Text [JSONEvent])]
valuesSetCases =
  [ ("sets every object value", "values", "0", "{\"a\":1,\"b\":2}", Right [JSONBeginObject, JSONObjectKey "a", JSONNumber 0, JSONObjectKey "b", JSONNumber 0, JSONEndObject])
  ]

valuesDeleteCases :: [(String, Text, Text, Either Text [JSONEvent])]
valuesDeleteCases =
  [ ("removes every member", "values", "{\"a\":1,\"b\":2}", Right [JSONBeginObject, JSONEndObject]),
    ("leaves arrays unchanged on delete", "values", "[1,2]", Right [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONEndArray])
  ]

-------------------------------------------------------------------------------
-- ix rewrite (arrays only)
-------------------------------------------------------------------------------

ixRewriteSpec :: Spec
ixRewriteSpec = describe "ix rewrite" $ do
  forM_ ixOverCases $ \(label, optic, t, doc, expected) ->
    it label $ runOverTest optic t doc `shouldReturn` expected
  forM_ ixDeleteCases $ \(label, optic, doc, expected) ->
    it label $ runDeleteTest optic doc `shouldReturn` expected

ixOverCases :: [(String, Text, Transformation, Text, Either Text [JSONEvent])]
ixOverCases =
  [ ("adds to the indexed element", "ix 1", Add 10, "[1,2,3]", Right [JSONBeginArray, JSONNumber 1, JSONNumber 12, JSONNumber 3, JSONEndArray]),
    ("leaves objects unchanged", "ix 0", Add 1, "{\"a\":1}", Right [JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject])
  ]

ixDeleteCases :: [(String, Text, Text, Either Text [JSONEvent])]
ixDeleteCases =
  [ ("leaves objects unchanged on delete", "ix 0", "{\"a\":1}", Right [JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject])
  ]

-------------------------------------------------------------------------------
-- filter rewrite
-------------------------------------------------------------------------------

filterRewriteSpec :: Spec
filterRewriteSpec = describe "filter rewrite" $ do
  forM_ filterOverCases $ \(label, optic, t, doc, expected) ->
    it label $ runOverTest optic t doc `shouldReturn` expected
  forM_ filterSetCases $ \(label, optic, value, doc, expected) ->
    it label $ runSetTest optic value doc `shouldReturn` expected
  forM_ filterDeleteCases $ \(label, optic, doc, expected) ->
    it label $ runDeleteTest optic doc `shouldReturn` expected

filterOverCases :: [(String, Text, Transformation, Text, Either Text [JSONEvent])]
filterOverCases =
  [ ("replaces kept values", "each . filter @age == 30", Const (Number 0), "[{\"age\":30},{\"age\":20}]", Right [JSONBeginArray, JSONNumber 0, JSONBeginObject, JSONObjectKey "age", JSONNumber 20, JSONEndObject, JSONEndArray]),
    ("rewrites through a filter into kept values", "each . filter @age == 30 . @score", Add 100, "[{\"age\":30,\"score\":1},{\"age\":20,\"score\":2}]", Right [JSONBeginArray, JSONBeginObject, JSONObjectKey "age", JSONNumber 30, JSONObjectKey "score", JSONNumber 101, JSONEndObject, JSONBeginObject, JSONObjectKey "age", JSONNumber 20, JSONObjectKey "score", JSONNumber 2, JSONEndObject, JSONEndArray]),
    ("fails when the predicate does not fit", "each . filter @a +1", Const (Number 0), "[{\"a\":\"x\"}]", Left "expected a number"),
    ("fails when the predicate is not boolean", "each . filter @a +1", Const (Number 0), "[{\"a\":1}]", Left "filter transformation must produce a boolean")
  ]

filterSetCases :: [(String, Text, Text, Text, Either Text [JSONEvent])]
filterSetCases =
  [ ("sets kept values", "each . filter @age == 30", "0", "[{\"age\":30},{\"age\":20}]", Right [JSONBeginArray, JSONNumber 0, JSONBeginObject, JSONObjectKey "age", JSONNumber 20, JSONEndObject, JSONEndArray])
  ]

filterDeleteCases :: [(String, Text, Text, Either Text [JSONEvent])]
filterDeleteCases =
  [ ("removes kept elements", "each . filter @age == 30", "[{\"age\":30},{\"age\":20}]", Right [JSONBeginArray, JSONBeginObject, JSONObjectKey "age", JSONNumber 20, JSONEndObject, JSONEndArray]),
    ("removes kept members but keeps the container", "@users . each . filter @age == 30", "{\"users\":[{\"age\":30},{\"age\":20}],\"b\":2}", Right [JSONBeginObject, JSONObjectKey "users", JSONBeginArray, JSONBeginObject, JSONObjectKey "age", JSONNumber 20, JSONEndObject, JSONEndArray, JSONObjectKey "b", JSONNumber 2, JSONEndObject]),
    ("removes the whole member when the gate keeps it", "@users . filter (each) (== 1)", "{\"users\":[1],\"b\":2}", Right [JSONBeginObject, JSONObjectKey "b", JSONNumber 2, JSONEndObject]),
    ("removes the whole document when the gate keeps it", "filter @age == 30", "{\"age\":30}", Right []),
    ("leaves the document unchanged when the gate drops it", "filter @age == 30", "{\"age\":20}", Right [JSONBeginObject, JSONObjectKey "age", JSONNumber 20, JSONEndObject])
  ]

chunkedRewriteSpec :: Spec
chunkedRewriteSpec = describe "chunked input" $ do
  it "rewrites agree with whole-input runs under every split" $ do
    forM_ overChunkCases $ \(opticStr, transformation, doc) -> do
      expected <- runOverTest opticStr transformation doc
      case parseOptic opticStr of
        Left err -> expectationFailure $ "bad optic: " <> show err
        Right optic ->
          forM_ (splits doc) $ \chunks -> do
            actual <- runRewriteChunks (\early -> runOver early optic transformation testConfig) chunks
            actual `shouldBe` expected

  it "deletes agree with whole-input runs under every split" $ do
    forM_ deleteChunkCases $ \(opticStr, doc) ->
      case parseOptic opticStr of
        Left err -> expectationFailure $ "bad optic: " <> show err
        Right optic -> do
          expected <- runDeleteTest opticStr doc
          forM_ (splits doc) $ \chunks -> do
            actual <- runRewriteChunks (\early -> runDelete early optic testConfig) chunks
            actual `shouldBe` expected

overChunkCases :: [(Text, Transformation, Text)]
overChunkCases =
  [ ("@users.each.@age", Add 1, "{\"users\":[{\"age\":1},{\"age\":2}]}"),
    ("each", Add 1, "[1,2,3]"),
    ("each . _String", ConcatString "!", "[1,\"a\",\"b\"]"),
    ("keys", ConcatString "!", "{\"a\":1,\"b\":2}"),
    ("values", Add 1, "{\"a\":1,\"b\":2}"),
    ("ix 1", Add 1, "[1,2,3]"),
    ("each . filter @age == 30", Const (Number 0), "[{\"age\":30},{\"age\":20}]")
  ]

deleteChunkCases :: [(Text, Text)]
deleteChunkCases =
  [ ("@users.each.@name", "{\"users\":[{\"name\":\"a\",\"age\":1},{\"age\":2}]}"),
    ("each", "[1,2,3]"),
    ("@a", "{\"a\":{\"b\":[1,2]},\"c\":3}"),
    ("keys", "{\"a\":1,\"b\":2}"),
    ("values", "{\"a\":1,\"b\":2}"),
    ("each . filter @age == 30", "[{\"age\":30},{\"age\":20}]")
  ]

-- | Run a rewrite continuation over every top-level document and
-- collect the raw output bytes. Multi-document output is not a single
-- value, so unlike 'runRewriteChunks' this does not reparse to events.
runRewriteDocumentsTest :: (Early HQError -> RewriteContinuation) -> [Text] -> IO (Either Text Text)
runRewriteDocumentsTest mkRun chunks = do
  result <-
    runEarly
      ( \early -> do
          (byteChunks :> _) <- S.toList (rewriteDocuments early (mkRun early) cursor initialEmitState)
          pure (decodeUtf8 (mconcat byteChunks))
      )
  pure (first renderHQError result)
  where
    textStream :: StreamIO Text ()
    textStream = S.each chunks
    cursor = Cursor [] initialDecoder textStream

multiDocumentRewriteSpec :: Spec
multiDocumentRewriteSpec = describe "multiple documents" $ do
  it "rewrites the same field across documents" $ do
    Right optic <- pure (parseOptic "@a")
    runRewriteDocumentsTest (\early -> runOver early optic (Add 1) testConfig) ["{\"a\":1} {\"a\":2}"]
      `shouldReturn` Right "{\n  \"a\": 2\n}\n{\n  \"a\": 3\n}\n"

  it "deletes across documents" $ do
    Right optic <- pure (parseOptic "@x")
    runRewriteDocumentsTest (\early -> runDelete early optic testConfig) ["{\"a\":1,\"x\":0} {\"b\":2}"]
      `shouldReturn` Right "{\n  \"a\": 1\n}\n{\n  \"b\": 2\n}\n"

  it "continues across chunk splits at document boundaries" $ do
    Right optic <- pure (parseOptic "@a")
    runRewriteDocumentsTest (\early -> runOver early optic (Add 1) testConfig) ["{\"a\":1} {\"a", "\":2}"]
      `shouldReturn` Right "{\n  \"a\": 2\n}\n{\n  \"a\": 3\n}\n"

  it "fails on trailing garbage" $ do
    Right optic <- pure (parseOptic "@a")
    runRewriteDocumentsTest (\early -> runOver early optic (Add 1) testConfig) ["{\"a\":0} garbage"]
      `shouldReturn` Left "unexpected character 'g' in JSON input"
