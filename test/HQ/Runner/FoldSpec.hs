{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner.FoldSpec (spec) where

import HQ.Early (Early, runEarly)
import HQ.Error (HQError (..), renderHQError)
import HQ.JSON.Decoder (StreamIO, initialDecoder)
import HQ.JSON.Event (JSONEvent (..), valueToEvents)
import HQ.JSON.Parser (parseValue)
import HQ.Optic (Optic)
import HQ.Optic.Parser (parseOptic)
import HQ.Runner (foldDocuments)
import HQ.Runner.Cursor (Cursor (..), EventStream)
import HQ.Runner.Fold (focusMany, runFold, runPreview)
import HQ.Transformation.Error (renderTransformationError)
import Relude hiding (Compose, id, many, not, or, some, subtract, toStrict)
import Streaming.Prelude qualified as S
import Test.HQ (splits)
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
  multiDocumentSpec

-- | Run an optic runner against chunked JSON text and collect output
-- events. Every fold/preview harness shares it; they differ only in
-- the runner and the chunks. Runs in IO because the streaming decoder
-- and cursor use IO internally.
runOnChunks :: (Early HQError -> Cursor -> EventStream Cursor) -> [Text] -> IO (Either Text [JSONEvent])
runOnChunks action chunks = do
  result <- runEarly (\early -> S.toList_ $ action early cursor)
  pure (first renderHQError result)
  where
    textStream :: StreamIO Text ()
    textStream = S.each chunks
    cursor = Cursor [] initialDecoder textStream

-- | Run a fold optic against a JSON text input and collect output events.
runFoldTest :: Optic -> Text -> IO (Either Text [JSONEvent])
runFoldTest optic input = runOnChunks (`runFold` optic) [input]

-- | Parse an optic string and run it against JSON input.
runQueryTest :: Text -> Text -> IO (Either Text [JSONEvent])
runQueryTest opticStr jsonInput = case parseOptic opticStr of
  Left err -> pure (Left (show err))
  Right optic -> runFoldTest optic jsonInput

-- | Run a preview optic against a JSON text input and collect the
-- (at most one) output value's events.
runPreviewTest :: Optic -> Text -> IO (Either Text [JSONEvent])
runPreviewTest optic input = runOnChunks (`runPreview` optic) [input]

-- | Parse an optic string and preview it against JSON input.
runQueryPreviewTest :: Text -> Text -> IO (Either Text [JSONEvent])
runQueryPreviewTest opticStr jsonInput = case parseOptic opticStr of
  Left err -> pure (Left (show err))
  Right optic -> runPreviewTest optic jsonInput

-- | Run a fold optic against chunked JSON text.
runFoldChunks :: Optic -> [Text] -> IO (Either Text [JSONEvent])
runFoldChunks optic = runOnChunks (`runFold` optic)

-- | Run a preview optic against chunked JSON text.
runPreviewChunks :: Optic -> [Text] -> IO (Either Text [JSONEvent])
runPreviewChunks optic = runOnChunks (`runPreview` optic)

-- | Parse an optic string and preview it against chunked JSON input.
runQueryPreviewChunks :: Text -> [Text] -> IO (Either Text [JSONEvent])
runQueryPreviewChunks opticStr chunks = case parseOptic opticStr of
  Left err -> pure (Left (show err))
  Right optic -> runPreviewChunks optic chunks

-- | Run a fold optic over every top-level document across text chunks.
runFoldDocumentsTest :: Optic -> [Text] -> IO (Either Text [JSONEvent])
runFoldDocumentsTest optic = runOnChunks (`foldDocuments` optic)

-- | Parse an optic string and run it over every top-level document.
runQueryDocumentsTest :: Text -> [Text] -> IO (Either Text [JSONEvent])
runQueryDocumentsTest opticStr chunks = case parseOptic opticStr of
  Left err -> pure (Left (show err))
  Right optic -> runFoldDocumentsTest optic chunks

--------------------------------------------------------------------------------
-- preview
--------------------------------------------------------------------------------

previewSpec :: Spec
previewSpec = describe "preview" $ forM_ previewCases $ \(label, optic, doc, expected) ->
  it label $ runQueryPreviewTest optic doc `shouldReturn` expected

previewCases :: [(String, Text, Text, Either Text [JSONEvent])]
previewCases =
  [ ("returns a field value", "@name", "{\"name\":\"alice\",\"age\":30}", Right [JSONString "alice"]),
    ("returns nothing when the field is missing", "@name", "{\"age\":30}", Right []),
    ("returns nothing for non-object input", "@name", "42", Right []),
    ("returns the first element of an array", "each", "[1,2,3]", Right [JSONNumber 1]),
    ("returns the first object value", "each", "{\"a\":1,\"b\":2}", Right [JSONNumber 1]),
    ("returns nothing for an empty array", "each", "[]", Right []),
    ("returns the first composed match", "each . @name", "[{\"name\":\"alice\"},{\"name\":\"bob\"}]", Right [JSONString "alice"]),
    -- NOTE: spaces around the outer '.' are load-bearing here;
    -- '== 30.@age' would lex '30.' as a number prefix.
    ("returns the first match through a filter", "each.filter @age == 30 . @age", "[{\"age\":20},{\"age\":30}]", Right [JSONNumber 30]),
    ("returns the first array element with ix 0", "ix 0", "[10,20]", Right [JSONNumber 10]),
    ("returns nothing when ix 0 finds no element", "ix 0", "[]", Right []),
    ("returns the proper array element with ix", "ix 0", "[0,1,2,3]", Right [JSONNumber 0]),
    ("returns the last array element with ix", "ix 3", "[0,1,2,3]", Right [JSONNumber 3]),
    ("returns nothing when ix finds no element", "ix 0", "[]", Right []),
    ("returns nothing when ix is out of bounds", "ix 4", "[0,1,2,3]", Right []),
    ("matches a string prism", "_String", "\"hello\"", Right [JSONString "hello"]),
    ("returns nothing when a prism does not match", "_String", "42", Right []),
    ("returns a whole container value", "@obj", "{\"obj\":{\"a\":1},\"next\":2}", Right [JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject]),
    ("returns the whole input for id", "id", "[1,2]", Right [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONEndArray]),
    ("stops reading input after the first match", "each", "[1, 2,,]", Right [JSONNumber 1])
  ]

--------------------------------------------------------------------------------
-- keys
--------------------------------------------------------------------------------

keysSpec :: Spec
keysSpec = describe "keys" $ do
  forM_ keysCases $ \(label, optic, doc, expected) ->
    it label $ runQueryTest optic doc `shouldReturn` expected
  forM_ keysPreviewCases $ \(label, optic, doc, expected) ->
    it label $ runQueryPreviewTest optic doc `shouldReturn` expected

keysCases :: [(String, Text, Text, Either Text [JSONEvent])]
keysCases =
  [ ("yields object keys as strings", "keys", "{\"a\":1,\"b\":2}", Right [JSONString "a", JSONString "b"]),
    ("yields nothing for arrays", "keys", "[10,20,30]", Right []),
    ("yields nothing for empty containers", "keys", "{}", Right []),
    ("yields nothing for empty arrays", "keys", "[]", Right []),
    ("yields nothing for number scalars", "keys", "42", Right []),
    ("yields nothing for string scalars", "keys", "\"x\"", Right []),
    ("composes keys with a matching prism", "keys . _String", "{\"a\":1}", Right [JSONString "a"]),
    ("composes keys with a mismatched prism", "keys . _Number", "{\"a\":1}", Right []),
    ("composes keys after a field", "@obj . keys", "{\"obj\":{\"a\":1}}", Right [JSONString "a"])
  ]

keysPreviewCases :: [(String, Text, Text, Either Text [JSONEvent])]
keysPreviewCases =
  [ ("previews the first key", "keys", "{\"a\":1,\"b\":2}", Right [JSONString "a"]),
    ("previews nothing for arrays", "keys", "[10,20]", Right [])
  ]

--------------------------------------------------------------------------------
-- values (objects only)
--------------------------------------------------------------------------------

valuesSpec :: Spec
valuesSpec = describe "values" $ do
  forM_ valuesCases $ \(label, optic, doc, expected) ->
    it label $ runQueryTest optic doc `shouldReturn` expected
  forM_ valuesPreviewCases $ \(label, optic, doc, expected) ->
    it label $ runQueryPreviewTest optic doc `shouldReturn` expected

valuesCases :: [(String, Text, Text, Either Text [JSONEvent])]
valuesCases =
  [ ("yields object values", "values", "{\"a\":1,\"b\":2}", Right [JSONNumber 1, JSONNumber 2]),
    ("yields nothing for arrays", "values", "[1,2,3]", Right []),
    ("yields nothing for empty objects", "values", "{}", Right []),
    ("yields nothing for scalars", "values", "42", Right []),
    ("yields nested containers whole", "values", "{\"a\":{\"b\":1}}", Right [JSONBeginObject, JSONObjectKey "b", JSONNumber 1, JSONEndObject]),
    ("composes with a field", "values . @x", "{\"a\":{\"x\":1},\"b\":{\"x\":2}}", Right [JSONNumber 1, JSONNumber 2]),
    ("returns empty when the field is missing in all values", "values . @x", "{\"a\":1}", Right [])
  ]

valuesPreviewCases :: [(String, Text, Text, Either Text [JSONEvent])]
valuesPreviewCases =
  [ ("previews the first value", "values", "{\"a\":1,\"b\":2}", Right [JSONNumber 1]),
    ("previews nothing for arrays", "values", "[1,2]", Right [])
  ]

--------------------------------------------------------------------------------
-- ix
--------------------------------------------------------------------------------

ixSpec :: Spec
ixSpec = describe "ix" $ forM_ ixCases $ \(label, optic, doc, expected) ->
  it label $ runQueryTest optic doc `shouldReturn` expected

ixCases :: [(String, Text, Text, Either Text [JSONEvent])]
ixCases =
  [ ("yields the first element", "ix 0", "[7,8]", Right [JSONNumber 7]),
    ("yields the second element", "ix 1", "[7,8]", Right [JSONNumber 8]),
    ("yields nothing when out of bounds", "ix 5", "[1,2]", Right []),
    ("yields nothing for objects", "ix 0", "{\"a\":1}", Right []),
    ("yields nothing for scalars", "ix 0", "42", Right [])
  ]

--------------------------------------------------------------------------------
-- filter
--------------------------------------------------------------------------------

filterSpec :: Spec
filterSpec = describe "filter" $ do
  forM_ filterCases $ \(label, optic, doc, expected) ->
    it label $ runQueryTest optic doc `shouldReturn` expected
  forM_ filterPreviewCases $ \(label, optic, doc, expected) ->
    it label $ runQueryPreviewTest optic doc `shouldReturn` expected

filterCases :: [(String, Text, Text, Either Text [JSONEvent])]
filterCases =
  [ ("keeps array elements when the test holds", "each . filter @age == 30", "[{\"age\":30},{\"age\":20}]", Right [JSONBeginObject, JSONObjectKey "age", JSONNumber 30, JSONEndObject]),
    ("keeps the whole value on any match", "filter each == 1", "[1,2,1]", Right [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONNumber 1, JSONEndArray]),
    ("drops values when nothing matches", "each . filter @age == 30", "[{\"age\":20}]", Right []),
    ("drops values with a missing focus", "filter @age == 30", "{\"b\":1}", Right []),
    ("filters matching scalars", "filter id == 1", "1", Right [JSONNumber 1]),
    ("filters mismatched scalars", "filter id == 1", "2", Right []),
    ("composes after a filter", "each . filter @age == 30 . @name", "[{\"age\":30,\"name\":\"a\"},{\"age\":20,\"name\":\"b\"}]", Right [JSONString "a"]),
    ("keeps nested filtered values", "filter (filter @a == 1) (== {\"a\":1})", "{\"a\":1}", Right [JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject]),
    ("fails when the predicate does not fit", "each . filter @a +1", "[{\"a\":\"x\"}]", Left "expected a number"),
    ("fails when the predicate is not boolean", "each . filter @a +1", "[{\"a\":1}]", Left "filter transformation must produce a boolean"),
    ("skips replayed containers looking for later members", "filter @a == 1 . @zzz", "{\"a\":1,\"big\":[1,2,3]}", Right [])
  ]

filterPreviewCases :: [(String, Text, Text, Either Text [JSONEvent])]
filterPreviewCases =
  [ ("previews the first kept value", "each . filter @age == 30", "[{\"age\":20},{\"age\":30}]", Right [JSONBeginObject, JSONObjectKey "age", JSONNumber 30, JSONEndObject])
  ]

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
            let expected = first renderTransformationError (concatMap valueToEvents <$> focusMany optic value)
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
    "@a",
    "@missing",
    "ix 0",
    "ix 2",
    "_String",
    "_Number",
    "_Just",
    "_Null",
    "each . @x",
    "@a . each",
    "filter @a == 1",
    "filter (each . @x) (== 2)",
    "each . filter @b == 2"
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
fieldSpec = describe "field" $ forM_ fieldCases $ \(label, optic, doc, expected) ->
  it label $ runQueryTest optic doc `shouldReturn` expected

fieldCases :: [(String, Text, Text, Either Text [JSONEvent])]
fieldCases =
  [ ("extracts a field from an object", "@name", "{\"name\":\"alice\"}", Right [JSONString "alice"]),
    ("returns empty when field is missing", "@name", "{\"age\":30}", Right []),
    ("returns empty for non-object input", "@name", "42", Right [])
  ]

--------------------------------------------------------------------------------
-- id
--------------------------------------------------------------------------------

idSpec :: Spec
idSpec = describe "id" $ forM_ idCases $ \(label, optic, doc, expected) ->
  it label $ runQueryTest optic doc `shouldReturn` expected

idCases :: [(String, Text, Text, Either Text [JSONEvent])]
idCases =
  [ ("returns the input value unchanged", "id", "42", Right [JSONNumber 42]),
    ("returns a string value unchanged", "id", "\"hello\"", Right [JSONString "hello"])
  ]

--------------------------------------------------------------------------------
-- each on arrays
--------------------------------------------------------------------------------

eachArraySpec :: Spec
eachArraySpec = describe "each on arrays" $ forM_ eachArrayCases $ \(label, optic, doc, expected) ->
  it label $ runQueryTest optic doc `shouldReturn` expected

eachArrayCases :: [(String, Text, Text, Either Text [JSONEvent])]
eachArrayCases =
  [ ("yields each element of a numeric array", "each", "[1,2,3]", Right [JSONNumber 1, JSONNumber 2, JSONNumber 3]),
    ("yields each element of a string array", "each", "[\"a\",\"b\",\"c\"]", Right [JSONString "a", JSONString "b", JSONString "c"]),
    ("yields a single-element array", "each", "[42]", Right [JSONNumber 42]),
    ("yields nothing for an empty array", "each", "[]", Right []),
    ("yields nothing for non-container input", "each", "42", Right []),
    ("yields nested array elements", "each", "[[1,2],[3]]", Right [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONEndArray, JSONBeginArray, JSONNumber 3, JSONEndArray])
  ]

--------------------------------------------------------------------------------
-- each on objects
--------------------------------------------------------------------------------

eachObjectSpec :: Spec
eachObjectSpec = describe "each on objects" $ forM_ eachObjectCases $ \(label, optic, doc, expected) ->
  it label $ runQueryTest optic doc `shouldReturn` expected

eachObjectCases :: [(String, Text, Text, Either Text [JSONEvent])]
eachObjectCases =
  [ ("yields each value of an object", "each", "{\"a\":1,\"b\":2}", Right [JSONNumber 1, JSONNumber 2]),
    ("yields nothing for an empty object", "each", "{}", Right [])
  ]

--------------------------------------------------------------------------------
-- each composed with field
--------------------------------------------------------------------------------

eachCompositionSpec :: Spec
eachCompositionSpec = describe "each . field composition" $ forM_ eachCompositionCases $ \(label, optic, doc, expected) ->
  it label $ runQueryTest optic doc `shouldReturn` expected

eachCompositionCases :: [(String, Text, Text, Either Text [JSONEvent])]
eachCompositionCases =
  [ ("extracts field from each array element", "each . @name", "[{\"name\":\"alice\"},{\"name\":\"bob\"}]", Right [JSONString "alice", JSONString "bob"]),
    ("extracts field from each object value", "each . @x", "{\"a\":{\"x\":1},\"b\":{\"x\":2}}", Right [JSONNumber 1, JSONNumber 2]),
    ("returns empty when field is missing in some elements", "each . @name", "[{\"name\":\"alice\"},{\"age\":30}]", Right [JSONString "alice"]),
    ("returns empty when field missing in all elements", "each . @name", "[{\"a\":1},{\"b\":2}]", Right []),
    ("extracts field from each element with multiple fields", "each . @id", "[{\"name\":\"alice\",\"id\":\"1\"},{\"name\":\"bob\",\"id\":\"2\"}]", Right [JSONString "1", JSONString "2"]),
    ("field . each distributes field then each", "@items . each", "{\"items\":[1,2,3]}", Right [JSONNumber 1, JSONNumber 2, JSONNumber 3]),
    ("each . each flattens nested arrays", "each . each", "[[1,2],[3,4]]", Right [JSONNumber 1, JSONNumber 2, JSONNumber 3, JSONNumber 4])
  ]

chunkedFoldSpec :: Spec
chunkedFoldSpec = describe "chunked input" $ do
  it "folds agree with whole-input runs under every split" $ do
    forM_ foldChunkCases $ \(opticStr, doc) ->
      case parseOptic opticStr of
        Left err -> expectationFailure $ "bad optic: " <> show err
        Right optic -> do
          expected <- runFoldTest optic doc
          forM_ (splits doc) $ \chunks -> do
            actual <- runFoldChunks optic chunks
            actual `shouldBe` expected

  it "preview never touches invalid tails under any split" $ do
    forM_ (splits "[1, 2,,]") $ \chunks -> do
      actual <- runQueryPreviewChunks "each" chunks
      actual `shouldBe` Right [JSONNumber 1]

  it "malformed inputs fail the same chunked as whole" $ do
    forM_ malformedChunkCases $ \(opticStr, doc) -> do
      expected <- runQueryTest opticStr doc
      case parseOptic opticStr of
        Left err -> expectationFailure $ "bad optic: " <> show err
        Right optic ->
          forM_ (splits doc) $ \chunks -> do
            actual <- runFoldChunks optic chunks
            actual `shouldBe` expected

foldChunkCases :: [(Text, Text)]
foldChunkCases =
  [ ("each . @name", "[{\"name\":\"alice\"},{\"age\":30}]"),
    ("@users.each.@name", "{\"users\":[{\"name\":\"a\",\"age\":1},{\"name\":\"b\"}]}"),
    ("each", "[[1,2],[3]]"),
    ("@a", "{\"a\":{\"b\":[1,2]},\"c\":3}"),
    ("@missing", "{\"a\":1}"),
    ("ix 2", "[1,2,3,4]"),
    ("each . each", "[[1,2],[3,4]]"),
    ("@a.@b", "{\"a\":{\"b\":[1,{\"c\":2}]}}"),
    ("keys", "{\"a\":1,\"b\":2}"),
    ("keys", "[10,20,30]"),
    ("keys . _String", "{\"a\":1}"),
    ("values", "{\"a\":1,\"b\":2}"),
    ("values . @x", "{\"a\":{\"x\":1},\"b\":2}"),
    ("ix 0", "{\"a\":1}"),
    ("each . filter @age == 30", "[{\"age\":30},{\"age\":20}]"),
    ("filter each == 1", "[1,2,1]"),
    ("each . filter @age == 30 . @name", "[{\"age\":30,\"name\":\"a\"}]"),
    -- Surrogate pairs inside containers: the take loops must step
    -- mid-value pauses (high surrogate) instead of finishing early.
    ("id", "[\"\\uD83D\\uDE00\"]"),
    ("id", "{\"u\":\"caf\\u00e9 \\uD83D\\uDE00\",\"s\":\"a/b\"}"),
    ("each", "[\"\\uD83D\\uDE00\",\"x\"]")
  ]

malformedChunkCases :: [(Text, Text)]
malformedChunkCases =
  [ ("@b", "{\"a\":1,\"b\":tru}"),
    ("@a", "{\"a\":1,\"b\":tru}"),
    ("each", "[1,,2]"),
    ("@a", "{\"a\":01}"),
    ("keys", "[1,,2]"),
    ("values", "{\"a\":tru}"),
    ("filter @a == 1", "{\"a\":1,\"b\":tru}"),
    ("id", "[\"\\uD83D\"]"),
    ("id", "[\"\\uD83D\\uxyz\"]")
  ]

multiDocumentSpec :: Spec
multiDocumentSpec = describe "multiple documents" $ do
  forM_ multiDocumentCases $ \(label, optic, chunks, expected) ->
    it label $ runQueryDocumentsTest optic chunks `shouldReturn` expected

  it "previews only the first document" $ do
    runQueryPreviewTest "@a" "{\"a\":1} {\"a\":2}"
      `shouldReturn` Right [JSONNumber 1]

multiDocumentCases :: [(String, Text, [Text], Either Text [JSONEvent])]
multiDocumentCases =
  [ ("folds the same field across documents", "@a", ["{\"a\":1} {\"a\":2}"], Right [JSONNumber 1, JSONNumber 2]),
    ("skips documents without a match", "@a", ["{\"a\":1} {\"b\":2} [3]"], Right [JSONNumber 1]),
    ("continues across chunk splits at document boundaries", "@a", ["{\"a\":1} {\"a", "\":2}"], Right [JSONNumber 1, JSONNumber 2]),
    ("fails on trailing garbage", "@a", ["{\"a\":1} garbage"], Left "unexpected character 'g' in JSON input"),
    ("yields nothing for empty input", "@a", [], Right []),
    ("yields nothing for whitespace-only input", "@a", ["  \n "], Right [])
  ]
