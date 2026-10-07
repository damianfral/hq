{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner.RewriteSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Text as T
import HQ.Early (Early, runEarly)
import HQ.Error (HQError (..), renderHQError)
import HQ.JSON.Decoder (StreamIO, initialDecoder)
import HQ.JSON.Encoder
import HQ.JSON.Event (JSONEvent (..), eventsToValue)
import HQ.JSON.Parser (parseValueEvents)
import HQ.Optic.Parser (parseOptic)
import HQ.Runner (rewriteDocuments)
import HQ.Runner.Cursor (Cursor (..), RewriteContinuation)
import HQ.Runner.Rewrite (runDelete, runOver)
import HQ.Transformation
import Relude hiding (Compose, Const, and, id, length, many, not, or, reverse, some, subtract, toStrict)
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
  multiDocumentRewriteSpec

-- | Encoder config for rewrite tests: pretty output, matching the
-- production default.
testConfig :: EncoderConfig
testConfig = EncoderConfig (Pretty 2) (ValueOptions NoRaw NoJoin)

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
          (outChunks :> _) <- S.toList (mkRun early cursor initialEncoderState)
          (byteChunks :> _) <- S.toList (encodeChunks 65536 (S.each outChunks))
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

-- | Parse an optic string and a replacement value, run @set@ against
-- JSON input, and collect the rewritten document's events.
--
-- @set@ is @over@ with a constant transformation, so the harness drives
-- 'runOver' with 'Const'.
runSetTest :: Text -> Text -> Text -> IO (Either Text [JSONEvent])
runSetTest opticStr valueStr jsonInput = case parseOptic opticStr of
  Left err -> pure (Left (show err))
  Right optic -> case parseValueEvents valueStr of
    Left err -> pure (Left err)
    Right events -> case eventsToValue events of
      Left err -> pure (Left (renderHQError (HQRunnerError err)))
      Right value ->
        runRewriteTest (\early -> runOver early optic (Const value) testConfig) jsonInput

-- | Parse an optic string and run @delete@ against JSON input,
-- collecting the rewritten document's events.
runDeleteTest :: Text -> Text -> IO (Either Text [JSONEvent])
runDeleteTest opticStr jsonInput = case parseOptic opticStr of
  Left err -> pure (Left (show err))
  Right optic -> runRewriteTest (\early -> runDelete early optic testConfig) jsonInput

-- | Parse an optic string and a transformation, run @over@ against JSON
-- input, and collect the rewritten document's events.
runOverTest :: Text -> Transformation -> Text -> IO (Either Text [JSONEvent])
runOverTest opticStr transformation jsonInput = case parseOptic opticStr of
  Left err -> pure (Left (show err))
  Right optic ->
    runRewriteTest (\early -> runOver early optic transformation testConfig) jsonInput

--------------------------------------------------------------------------------
-- set
--------------------------------------------------------------------------------

setSpec :: Spec
setSpec = describe "set" $ do
  it "replaces a field value" $ do
    runSetTest "@name" "\"bob\"" "{\"name\":\"alice\",\"age\":30}"
    `shouldReturn` Right
      [ JSONBeginObject,
        JSONObjectKey "name",
        JSONString "bob",
        JSONObjectKey "age",
        JSONNumber 30,
        JSONEndObject
      ]

  it "leaves the document unchanged when the field is missing" $ do
    runSetTest "@name" "5" "{\"age\":30}"
    `shouldReturn` Right
      [JSONBeginObject, JSONObjectKey "age", JSONNumber 30, JSONEndObject]

  it "leaves non-object input unchanged" $ do
    runSetTest "@name" "5" "[1,2]"
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
    runSetTest "@users.each.@name" "\"anon\"" "{\"users\":[{\"name\":\"a\",\"age\":1},{\"name\":\"b\"}]}"
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
    runSetTest "@users.each.@name" "\"anon\"" "{\"users\":[{\"age\":1}]}"
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
    $ runSetTest "@a" "5" "{\"a\":{\"b\":[1,2]},\"c\":3}"
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
    runDeleteTest "@name" "{\"name\":\"alice\",\"age\":30}"
    `shouldReturn` Right
      [JSONBeginObject, JSONObjectKey "age", JSONNumber 30, JSONEndObject]

  it "leaves the document unchanged when the field is missing" $ do
    runDeleteTest "@name" "{\"age\":30}"
    `shouldReturn` Right
      [JSONBeginObject, JSONObjectKey "age", JSONNumber 30, JSONEndObject]

  it "leaves non-object input unchanged" $ do
    runDeleteTest "@name" "[1,2]"
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
    runDeleteTest "@a._String" "{\"a\":\"x\",\"b\":\"y\"}"
    `shouldReturn` Right
      [JSONBeginObject, JSONObjectKey "b", JSONString "y", JSONEndObject]

  it "keeps a member whose value does not match the composed prism" $ do
    runDeleteTest "@a._String" "{\"a\":5,\"b\":\"y\"}"
    `shouldReturn` Right
      [ JSONBeginObject,
        JSONObjectKey "a",
        JSONNumber 5,
        JSONObjectKey "b",
        JSONString "y",
        JSONEndObject
      ]

  it "removes composed fields but keeps the container" $ do
    runDeleteTest "@users.each.@name" "{\"users\":[{\"name\":\"a\",\"age\":1},{\"age\":2}]}"
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
    runDeleteTest "@a.@b" "{\"a\":{\"c\":1},\"b\":2}"
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
    runOverTest "each" (Add 1) "[1,2,3]"
    `shouldReturn` Right
      [JSONBeginArray, JSONNumber 2, JSONNumber 3, JSONNumber 4, JSONEndArray]

  it "adds to a field value" $ do
    runOverTest "@age" (Add 1) "{\"name\":\"alice\",\"age\":30}"
    `shouldReturn` Right
      [ JSONBeginObject,
        JSONObjectKey "name",
        JSONString "alice",
        JSONObjectKey "age",
        JSONNumber 31,
        JSONEndObject
      ]

  it "appends to string values" $ do
    runOverTest "each . _String" (ConcatString "!") "[1,\"a\",\"b\"]"
    `shouldReturn` Right
      [ JSONBeginArray,
        JSONNumber 1,
        JSONString "a!",
        JSONString "b!",
        JSONEndArray
      ]

  it "trims string values" $ do
    runOverTest "each . _String" Trim "[\"  hi  \",5]"
    `shouldReturn` Right
      [JSONBeginArray, JSONString "hi", JSONNumber 5, JSONEndArray]

  it "replaces substrings in string values" $ do
    runOverTest "@name" (Replace "a" "e") "{\"name\":\"alice\"}"
    `shouldReturn` Right
      [JSONBeginObject, JSONObjectKey "name", JSONString "elice", JSONEndObject]

  it "maps values to booleans" $ do
    runOverTest "each" (Equal (Number 1)) "[1,2,1]"
    `shouldReturn` Right
      [ JSONBeginArray,
        JSONBool True,
        JSONBool False,
        JSONBool True,
        JSONEndArray
      ]

  it "negates boolean values" $ do
    runOverTest "each . _Bool" Not "[true,false]"
    `shouldReturn` Right
      [JSONBeginArray, JSONBool False, JSONBool True, JSONEndArray]

  it "disjoins two transformations with or" $ do
    runOverTest "each" (Or (Equal (Number 1)) (Equal (Number 3))) "[1,2,3]"
    `shouldReturn` Right
      [ JSONBeginArray,
        JSONBool True,
        JSONBool False,
        JSONBool True,
        JSONEndArray
      ]

  it "conjoins two transformations with and" $ do
    runOverTest "each" (And (Equal (Number 1)) (Equal (Number 1))) "[1,2]"
    `shouldReturn` Right [JSONBeginArray, JSONBool True, JSONBool False, JSONEndArray]

  it "conjoins to true when both branches hold" $ do
    runOverTest "each" (And (IsPrefixOf "a") (IsSuffixOf "c")) "[\"abc\",\"ab\"]"
    `shouldReturn` Right [JSONBeginArray, JSONBool True, JSONBool False, JSONEndArray]

  it "short-circuits and on false" $ do
    -- The right branch would fail on a number; a false left branch
    -- must return False without running it.
    runOverTest "each" (And (Equal (Number 2)) Not) "[1]"
    `shouldReturn` Right [JSONBeginArray, JSONBool False, JSONEndArray]

  it "exclusive-disjoins two transformations with Xor" $ do
    runOverTest "each" (Xor (IsPrefixOf "a") (IsSuffixOf "a")) "[\"a\",\"ab\",\"ba\",\"b\"]"
    `shouldReturn` Right
      [ JSONBeginArray,
        JSONBool False,
        JSONBool True,
        JSONBool True,
        JSONBool False,
        JSONEndArray
      ]

  it "fails and with a non-boolean left branch" $ do
    runOverTest "each" (And (Add 1) (Equal (Number 1))) "[0]"
    `shouldReturn` Left "expected a boolean result from the left side of and"

  it "fails Xor with a non-boolean branch" $ do
    runOverTest "each" (Xor (Equal (Number 1)) (Add 1)) "[1]"
    `shouldReturn` Left "expected a boolean result from each side of xor"

  it "composes transformations right-to-left" $ do
    runOverTest "each" (Compose (Equal (Number 3)) (Add 1)) "[1,2,3]"
    `shouldReturn` Right
      [ JSONBeginArray,
        JSONBool False,
        JSONBool True,
        JSONBool False,
        JSONEndArray
      ]

  it "rewrites values focused by a composed optic" $ do
    runOverTest "@users.each.@age" (Add 1) "{\"users\":[{\"age\":1},{\"age\":2}]}"
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
    runOverTest "id" (Equal (Number 1)) "[1,2]"
    `shouldReturn` Right [JSONBool False]

  it "fails when the transformation does not fit the value" $ do
    runOverTest "each" (Add 1) "[\"a\",1]"
    `shouldReturn` Left "expected a number"

  it "strips a matching prefix" $ do
    runOverTest "each" (StripPrefix "pre") "[\"prefix\",\"pre\",\"other\"]"
    `shouldReturn` Right
      [ JSONBeginArray,
        JSONString "fix",
        JSONString "",
        JSONString "other",
        JSONEndArray
      ]

  it "strips a matching suffix" $ do
    runOverTest "each" (StripSuffix ".json") "[\"a.json\",\"json\"]"
    `shouldReturn` Right
      [JSONBeginArray, JSONString "a", JSONString "json", JSONEndArray]

  it "tests prefix membership" $ do
    runOverTest "each" (IsPrefixOf "pre") "[\"prefix\",\"other\"]"
    `shouldReturn` Right
      [JSONBeginArray, JSONBool True, JSONBool False, JSONEndArray]

  it "tests suffix membership" $ do
    runOverTest "each" (IsSuffixOf ".json") "[\"a.json\",\"other\"]"
    `shouldReturn` Right
      [JSONBeginArray, JSONBool True, JSONBool False, JSONEndArray]

  it "tests infix membership" $ do
    runOverTest "each" (IsInfixOf "fix") "[\"prefix\",\"other\"]"
    `shouldReturn` Right
      [JSONBeginArray, JSONBool True, JSONBool False, JSONEndArray]

  it "tests array emptiness" $ do
    runOverTest "each" IsEmpty "[[],[1]]"
    `shouldReturn` Right
      [JSONBeginArray, JSONBool True, JSONBool False, JSONEndArray]

  it "computes array lengths" $ do
    runOverTest "each" ArrayLength "[[1,2],[]]"
    `shouldReturn` Right
      [JSONBeginArray, JSONNumber 2, JSONNumber 0, JSONEndArray]

  it "reverses arrays" $ do
    runOverTest "each" ArrayReverse "[[1,2,3]]"
    `shouldReturn` Right
      [ JSONBeginArray,
        JSONBeginArray,
        JSONNumber 3,
        JSONNumber 2,
        JSONNumber 1,
        JSONEndArray,
        JSONEndArray
      ]

  it "drops duplicate array elements" $ do
    runOverTest "each" ArrayUnique "[[1,1,2,1]]"
    `shouldReturn` Right
      [ JSONBeginArray,
        JSONBeginArray,
        JSONNumber 1,
        JSONNumber 2,
        JSONEndArray,
        JSONEndArray
      ]

  it "sorts array elements" $ do
    runOverTest "each" ArraySort "[[3,1,2]]"
    `shouldReturn` Right
      [ JSONBeginArray,
        JSONBeginArray,
        JSONNumber 1,
        JSONNumber 2,
        JSONNumber 3,
        JSONEndArray,
        JSONEndArray
      ]

  it "fails sort on scalar elements" $ do
    runOverTest "each" ArraySort "[\"b\",1,true,null]"
    `shouldReturn` Left "expected an array"

  it "sorts mixed values by type rank" $ do
    runOverTest "each" ArraySort "[[\"b\",1,true,null]]"
    `shouldReturn` Right
      [ JSONBeginArray,
        JSONBeginArray,
        JSONNull,
        JSONBool True,
        JSONNumber 1,
        JSONString "b",
        JSONEndArray,
        JSONEndArray
      ]

  it "sorts nested arrays lexicographically" $ do
    runOverTest "each" ArraySort "[[[2],[1],[1,2]]]"
    `shouldReturn` Right
      [ JSONBeginArray,
        JSONBeginArray,
        JSONBeginArray,
        JSONNumber 1,
        JSONEndArray,
        JSONBeginArray,
        JSONNumber 1,
        JSONNumber 2,
        JSONEndArray,
        JSONBeginArray,
        JSONNumber 2,
        JSONEndArray,
        JSONEndArray,
        JSONEndArray
      ]

  it "fails sort on non-arrays" $ do
    runOverTest "each" ArraySort "[1]"
    `shouldReturn` Left "expected an array"

  it "fails stripPrefix on numbers" $ do
    runOverTest "each" (StripPrefix "a") "[1]"
    `shouldReturn` Left "expected a string"

  it "fails length on strings" $ do
    runOverTest "each" ArrayLength "[\"a\"]"
    `shouldReturn` Left "expected an array"

-------------------------------------------------------------------------------
-- keys rewrite (object key renaming; array indices are read-only)
-------------------------------------------------------------------------------

keysRewriteSpec :: Spec
keysRewriteSpec = describe "keys rewrite" $ do
  it "renames every key" $ do
    runOverTest "keys" (ConcatString "!") "{\"a\":1,\"b\":2}"
    `shouldReturn` Right
      [ JSONBeginObject,
        JSONObjectKey "a!",
        JSONNumber 1,
        JSONObjectKey "b!",
        JSONNumber 2,
        JSONEndObject
      ]

  it "trims keys" $ do
    runOverTest "keys" Trim "{\" a \":1}"
    `shouldReturn` Right
      [JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject]

  it "renames through a composed string prism" $ do
    runOverTest "keys . _String" (ConcatString "!") "{\"a\":1}"
    `shouldReturn` Right
      [JSONBeginObject, JSONObjectKey "a!", JSONNumber 1, JSONEndObject]

  it "leaves keys alone when the prism does not match" $ do
    runOverTest "keys . _Number" (Add 1) "{\"a\":1}"
    `shouldReturn` Right
      [JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject]

  it "leaves arrays unchanged" $ do
    runOverTest "keys" (ConcatString "!") "[1,2]"
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
    runOverTest "keys" (Add 1) "{\"a\":1}"
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
    runOverTest "values" (Add 1) "{\"a\":1,\"b\":2}"
    `shouldReturn` Right
      [ JSONBeginObject,
        JSONObjectKey "a",
        JSONNumber 2,
        JSONObjectKey "b",
        JSONNumber 3,
        JSONEndObject
      ]

  it "leaves arrays unchanged" $ do
    runOverTest "values" (Add 1) "[1,2,3]"
    `shouldReturn` Right
      [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONNumber 3, JSONEndArray]

  it "rewrites values focused by a composed prism" $ do
    runOverTest "values . _Number" (Add 1) "{\"a\":1,\"b\":\"x\"}"
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
    runOverTest "ix 1" (Add 10) "[1,2,3]"
    `shouldReturn` Right
      [JSONBeginArray, JSONNumber 1, JSONNumber 12, JSONNumber 3, JSONEndArray]

  it "leaves objects unchanged" $ do
    runOverTest "ix 0" (Add 1) "{\"a\":1}"
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
    runOverTest "each . filter @age == 30" (Const (Number 0)) "[{\"age\":30},{\"age\":20}]"
    `shouldReturn` Right
      [JSONBeginArray, JSONNumber 0, JSONBeginObject, JSONObjectKey "age", JSONNumber 20, JSONEndObject, JSONEndArray]

  it "rewrites through a filter into kept values" $ do
    runOverTest "each . filter @age == 30 . @score" (Add 100) "[{\"age\":30,\"score\":1},{\"age\":20,\"score\":2}]"
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
    runSetTest "each . filter @age == 30" "0" "[{\"age\":30},{\"age\":20}]"
    `shouldReturn` Right
      [JSONBeginArray, JSONNumber 0, JSONBeginObject, JSONObjectKey "age", JSONNumber 20, JSONEndObject, JSONEndArray]

  it "removes kept elements" $ do
    runDeleteTest "each . filter @age == 30" "[{\"age\":30},{\"age\":20}]"
    `shouldReturn` Right
      [JSONBeginArray, JSONBeginObject, JSONObjectKey "age", JSONNumber 20, JSONEndObject, JSONEndArray]

  it "removes kept members but keeps the container" $ do
    runDeleteTest "@users . each . filter @age == 30" "{\"users\":[{\"age\":30},{\"age\":20}],\"b\":2}"
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
    runDeleteTest "@users . filter (each == 1)" "{\"users\":[1],\"b\":2}"
    `shouldReturn` Right
      [JSONBeginObject, JSONObjectKey "b", JSONNumber 2, JSONEndObject]

  it "removes the whole document when the gate keeps it" $ do
    runDeleteTest "filter @age == 30" "{\"age\":30}" `shouldReturn` Right []

  it "leaves the document unchanged when the gate drops it" $ do
    runDeleteTest "filter @age == 30" "{\"age\":20}"
    `shouldReturn` Right
      [JSONBeginObject, JSONObjectKey "age", JSONNumber 20, JSONEndObject]

  it "fails when the predicate does not fit" $ do
    runOverTest "each . filter @a +1" (Const (Number 0)) "[{\"a\":\"x\"}]"
    `shouldReturn` Left "expected a number"

  it "fails when the predicate is not boolean" $ do
    runOverTest "each . filter @a +1" (Const (Number 0)) "[{\"a\":1}]"
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
            actual <- runRewriteChunks (\early -> runOver early optic transformation testConfig) chunks
            actual `shouldBe` expected

  it "deletes agree with whole-input runs under every split" $ do
    forM_ deleteChunkCases $ \(opticStr, doc) ->
      case parseOptic opticStr of
        Left err -> expectationFailure $ "bad optic: " <> show err
        Right optic -> do
          expected <- runDeleteTest opticStr doc
          forM_ (chunkSplits doc) $ \chunks -> do
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
          (outChunks :> _) <- S.toList (rewriteDocuments early (mkRun early) cursor initialEncoderState)
          (byteChunks :> _) <- S.toList (encodeChunks 65536 (S.each outChunks))
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
      `shouldReturn` Left "UnexpectedChar 'g'"
