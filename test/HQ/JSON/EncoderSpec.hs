{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.JSON.EncoderSpec (spec) where

import Data.Text qualified as T
import HQ.JSON.Encoder
import HQ.JSON.Event (JSONEvent (..))
import Relude hiding (Compose, id)
import Streaming (Of (..))
import Streaming.Prelude qualified as S
import Test.HQ (decodeChunks)
import Test.Syd

spec :: Spec
spec = describe "HQ.JSON.Encoder" $ do
  encodeSpec
  encodePrettySpec
  valueOptionsSpec
  roundtripSpec

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Encode events to a single Text value using compact style, without
-- separators between top-level values.
encodeEvents :: [JSONEvent] -> Text
encodeEvents = encodeEventsWith $ EncoderConfig Compact $ ValueOptions NoRaw Join

-- | Encode events with the given 'EncoderConfig' to a single Text
-- value.
encodeEventsWith :: EncoderConfig -> [JSONEvent] -> Text
encodeEventsWith encConfig events = runIdentity $ do
  result <- S.toList (encode encConfig 32 (S.each events))
  case result of
    chunks :> _ -> pure $ decodeUtf8 $ mconcat chunks

--------------------------------------------------------------------------------
-- encode
--------------------------------------------------------------------------------

encodeSpec :: Spec
encodeSpec = describe "encode" $ forM_ encodeCases $ \(label, events, expected) ->
  it label $ encodeEvents events `shouldBe` expected

encodeCases :: [(String, [JSONEvent], Text)]
encodeCases =
  [ ("encodes null", [JSONNull], "null"),
    ("encodes true", [JSONBool True], "true"),
    ("encodes false", [JSONBool False], "false"),
    ("encodes number", [JSONNumber 42], "42"),
    ("encodes string", [JSONString "hello"], "\"hello\""),
    ("encodes empty object", [JSONBeginObject, JSONEndObject], "{}"),
    ("encodes object with one field", [JSONBeginObject, JSONObjectKey "name", JSONString "alice", JSONEndObject], "{\"name\":\"alice\"}"),
    ("encodes object with multiple fields", [JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONObjectKey "b", JSONNumber 2, JSONEndObject], "{\"a\":1,\"b\":2}"),
    ("encodes empty array", [JSONBeginArray, JSONEndArray], "[]"),
    ("encodes array with one element", [JSONBeginArray, JSONNumber 1, JSONEndArray], "[1]"),
    ("encodes array with multiple elements", [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONNumber 3, JSONEndArray], "[1,2,3]"),
    ("encodes array of strings", [JSONBeginArray, JSONString "a", JSONString "b", JSONEndArray], "[\"a\",\"b\"]"),
    ("encodes nested objects", [JSONBeginObject, JSONObjectKey "a", JSONBeginObject, JSONObjectKey "b", JSONNumber 1, JSONEndObject, JSONEndObject], "{\"a\":{\"b\":1}}"),
    ("encodes nested arrays", [JSONBeginArray, JSONBeginArray, JSONNumber 1, JSONEndArray, JSONBeginArray, JSONNumber 2, JSONEndArray, JSONEndArray], "[[1],[2]]"),
    ("encodes object containing array", [JSONBeginObject, JSONObjectKey "items", JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONEndArray, JSONEndObject], "{\"items\":[1,2]}"),
    ("encodes array containing object", [JSONBeginArray, JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject, JSONEndArray], "[{\"a\":1}]"),
    ("encodes complex structure", [JSONBeginObject, JSONObjectKey "users", JSONBeginArray, JSONBeginObject, JSONObjectKey "name", JSONString "alice", JSONObjectKey "age", JSONNumber 30, JSONEndObject, JSONBeginObject, JSONObjectKey "name", JSONString "bob", JSONObjectKey "age", JSONNumber 25, JSONEndObject, JSONEndArray, JSONObjectKey "count", JSONNumber 2, JSONEndObject], "{\"users\":[{\"name\":\"alice\",\"age\":30},{\"name\":\"bob\",\"age\":25}],\"count\":2}")
  ]

--------------------------------------------------------------------------------
-- pretty encode
--------------------------------------------------------------------------------

encodePrettySpec :: Spec
encodePrettySpec = describe "pretty encode" $ do
  let prettyConfig = EncoderConfig (Pretty 2) $ ValueOptions NoRaw Join
  let compactConfig = EncoderConfig Compact $ ValueOptions NoRaw Join

  describe "scalars are identical in compact and pretty" $ do
    let scalarCases =
          [ ("null", [JSONNull], "null"),
            ("true", [JSONBool True], "true"),
            ("false", [JSONBool False], "false"),
            ("42", [JSONNumber 42], "42"),
            ("1.5", [JSONNumber 1.5], "1.5"),
            ("\"hello\"", [JSONString "hello"], "\"hello\"")
          ]
    forM_ scalarCases $ \(label, events, expected) ->
      it label $ do
        encodeEventsWith compactConfig events `shouldBe` expected
        encodeEventsWith prettyConfig events `shouldBe` expected

  describe "empty containers stay compact" $ do
    forM_ emptyContainerCases $ \(label, events, expected) ->
      it label $ encodeEventsWith prettyConfig events `shouldBe` expected

  describe "single-element containers" $ do
    forM_ singleElementCases $ \(label, events, expected) ->
      it label $ encodeEventsWith prettyConfig events `shouldBe` expected

  describe "multiple elements" $ do
    forM_ multipleElementCases $ \(label, events, expected) ->
      it label $ encodeEventsWith prettyConfig events `shouldBe` expected

  describe "nested structures" $ do
    forM_ nestedCases $ \(label, events, expected) ->
      it label $ encodeEventsWith prettyConfig events `shouldBe` expected

  describe "string escaping is preserved" $ do
    it "special characters encode identically with surrounding formatting" $ do
      let events =
            [ JSONBeginArray,
              JSONString "quote\" backslash\\ newline\n return\r tab\t bell\b formfeed\f control\x01 snowman☃ snow❄",
              JSONEndArray
            ]
      encodeEvents events
        `shouldBe` "[\"quote\\\" backslash\\\\ newline\\n return\\r tab\\t bell\\b formfeed\\f control\\u0001 snowman☃ snow❄\"]"
      encodeEventsWith prettyConfig events
        `shouldBe` "[\n  \"quote\\\" backslash\\\\ newline\\n return\\r tab\\t bell\\b formfeed\\f control\\u0001 snowman☃ snow❄\"\n]"
    it "pretty output decodes back to the same events" $ do
      let events =
            [ JSONBeginArray,
              JSONString "tab\tyes\nline2\r\n\"quoted\"\\and 日本語☃\x07",
              JSONNumber (-1.5e-3),
              JSONEndArray
            ]
          encoded = encodeEventsWith prettyConfig events
      case decodeChunks [encoded] of
        Left err -> expectationFailure $ "Re-decode failed: " <> show err
        Right evts' -> evts' `shouldBe` events

  describe "chunk sizes do not affect pretty output" $ do
    let events =
          [ JSONBeginObject,
            JSONObjectKey "users",
            JSONBeginArray,
            JSONBeginObject,
            JSONObjectKey "name",
            JSONString "alice",
            JSONObjectKey "age",
            JSONNumber 30,
            JSONEndObject,
            JSONBeginObject,
            JSONObjectKey "name",
            JSONString "bob",
            JSONObjectKey "age",
            JSONNumber 25,
            JSONEndObject,
            JSONEndArray,
            JSONObjectKey "tags",
            JSONBeginArray,
            JSONString "a\"b",
            JSONNull,
            JSONEndArray,
            JSONEndObject
          ]
        expected = encodeEventsWith prettyConfig events
    forM_ [1, 2, 3, 5, 8, 16, 24, 40] $ \cSize ->
      it ("chunk size " <> show cSize) $ do
        let concatenated = runIdentity $ do
              result <- S.toList (encode prettyConfig cSize (S.each events))
              case result of
                chunks :> _ -> pure $ decodeUtf8 $ mconcat chunks
        concatenated `shouldBe` expected
        case decodeChunks [concatenated] of
          Left err -> expectationFailure $ "Re-decode failed at chunk size " <> show cSize <> ": " <> show err
          Right evts' -> evts' `shouldBe` events

  describe "roundtrip through pretty encoding" $ do
    let prettyRoundtripCases =
          [ ("empty object", "{}"),
            ("empty array", "[]"),
            ("object", "{\"name\":\"Alice\"}"),
            ("array", "[1,2,3]"),
            ("nested", "{\"users\":[{\"name\":\"alice\"},{\"name\":\"bob\"}]}"),
            ("mixed array", "[1,true,null,\"x\",-2.5]"),
            ("escapes", "\"a\\nb\\\"c\\\\d\""),
            ("unicode", "\"日本語☃\""),
            ("number with exponent", "1e10"),
            ("negative exponent", "1e-2")
          ]
    forM_ prettyRoundtripCases $ \(label, input) ->
      it ("pretty roundtrips " <> label) $ prettyRoundtrip input

emptyContainerCases :: [(String, [JSONEvent], Text)]
emptyContainerCases =
  [ ("empty object", [JSONBeginObject, JSONEndObject], "{}"),
    ("empty array", [JSONBeginArray, JSONEndArray], "[]"),
    ("empty containers nested in non-empty containers", [JSONBeginObject, JSONObjectKey "a", JSONBeginArray, JSONEndArray, JSONObjectKey "b", JSONBeginObject, JSONEndObject, JSONEndObject], "{\n  \"a\": [],\n  \"b\": {}\n}"),
    ("empty arrays nested in an array", [JSONBeginArray, JSONBeginArray, JSONEndArray, JSONBeginArray, JSONEndArray, JSONEndArray], "[\n  [],\n  []\n]")
  ]

singleElementCases :: [(String, [JSONEvent], Text)]
singleElementCases =
  [ ("object", [JSONBeginObject, JSONObjectKey "name", JSONString "Alice", JSONEndObject], "{\n  \"name\": \"Alice\"\n}"),
    ("array", [JSONBeginArray, JSONNumber 42, JSONEndArray], "[\n  42\n]")
  ]

multipleElementCases :: [(String, [JSONEvent], Text)]
multipleElementCases =
  [ ("object with multiple fields", [JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONObjectKey "b", JSONNumber 2, JSONEndObject], "{\n  \"a\": 1,\n  \"b\": 2\n}"),
    ("array with multiple elements", [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONNumber 3, JSONEndArray], "[\n  1,\n  2,\n  3\n]")
  ]

nestedCases :: [(String, [JSONEvent], Text)]
nestedCases =
  [ ("object in object", [JSONBeginObject, JSONObjectKey "a", JSONBeginObject, JSONObjectKey "b", JSONNumber 1, JSONEndObject, JSONEndObject], "{\n  \"a\": {\n    \"b\": 1\n  }\n}"),
    ("array in array", [JSONBeginArray, JSONBeginArray, JSONNumber 1, JSONEndArray, JSONEndArray], "[\n  [\n    1\n  ]\n]"),
    ("array in object", [JSONBeginObject, JSONObjectKey "users", JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONEndArray, JSONEndObject], "{\n  \"users\": [\n    1,\n    2\n  ]\n}"),
    ("object in array", [JSONBeginArray, JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject, JSONEndArray], "[\n  {\n    \"a\": 1\n  }\n]"),
    ("deeply nested", [JSONBeginObject, JSONObjectKey "a", JSONBeginObject, JSONObjectKey "b", JSONBeginArray, JSONBeginObject, JSONObjectKey "c", JSONNumber 1, JSONEndObject, JSONEndArray, JSONEndObject, JSONEndObject], "{\n  \"a\": {\n    \"b\": [\n      {\n        \"c\": 1\n      }\n    ]\n  }\n}"),
    ("complex structure", [JSONBeginObject, JSONObjectKey "users", JSONBeginArray, JSONBeginObject, JSONObjectKey "name", JSONString "alice", JSONObjectKey "age", JSONNumber 30, JSONEndObject, JSONBeginObject, JSONObjectKey "name", JSONString "bob", JSONObjectKey "age", JSONNumber 25, JSONEndObject, JSONEndArray, JSONObjectKey "count", JSONNumber 2, JSONEndObject], "{\n  \"users\": [\n    {\n      \"name\": \"alice\",\n      \"age\": 30\n    },\n    {\n      \"name\": \"bob\",\n      \"age\": 25\n    }\n  ],\n  \"count\": 2\n}"),
    ("mixed types in array", [JSONBeginArray, JSONNumber 1, JSONString "hello", JSONBool True, JSONNull, JSONEndArray], "[\n  1,\n  \"hello\",\n  true,\n  null\n]")
  ]

--------------------------------------------------------------------------------
-- value output options
--------------------------------------------------------------------------------

valueOptionsSpec :: Spec
valueOptionsSpec = describe "value output options" $ do
  describe "newline separation" $ do
    let encConfig = EncoderConfig Compact $ ValueOptions NoRaw NoJoin
    forM_ newlineCases $ \(label, events, expected) ->
      it label $ encodeEventsWith encConfig events `shouldBe` expected
    it "applies pretty formatting inside each value" $ do
      encodeEventsWith
        (EncoderConfig (Pretty 2) $ ValueOptions NoRaw NoJoin)
        [ JSONBeginObject,
          JSONObjectKey "a",
          JSONNumber 1,
          JSONEndObject,
          JSONString "x"
        ]
      `shouldBe` "{\n  \"a\": 1\n}\n\"x\"\n"
    it "matches plain encode with separation disabled" $ do
      let events =
            [ JSONBeginObject,
              JSONObjectKey "a",
              JSONNumber 1,
              JSONEndObject,
              JSONString "x"
            ]
      encodeEventsWith (EncoderConfig Compact $ ValueOptions NoRaw Join) events
        `shouldBe` encodeEvents events

    it "output is independent of chunk size" $ do
      let events =
            [ JSONBeginObject,
              JSONObjectKey "a",
              JSONNumber 1,
              JSONEndObject,
              JSONString "x",
              JSONBeginArray,
              JSONNumber 2,
              JSONEndArray
            ]
          expected = encodeEventsWith encConfig events
      forM_ [1, 2, 3, 5, 8, 16] $ \flushSize -> do
        let concatenated = runIdentity $ do
              result <- S.toList (encode encConfig flushSize (S.each events))
              case result of
                chunks :> _ -> pure $ decodeUtf8 $ mconcat chunks
        concatenated `shouldBe` expected
    it "every line decodes back to the original events" $ do
      let events =
            [ JSONBeginObject,
              JSONObjectKey "a",
              JSONNumber 1,
              JSONEndObject,
              JSONString "x",
              JSONBeginArray,
              JSONNumber 2,
              JSONEndArray
            ]
          encoded = encodeEventsWith encConfig events
          lines' = filter (not . T.null) (T.splitOn "\n" encoded)
          decodedLines = map (fromRight [] . decodeChunks . pure) lines'
      mconcat decodedLines `shouldBe` events

  describe "raw string output"
    $ forM_ rawCases
    $ \(label, config, events, expected) ->
      it label $ encodeEventsWith config events `shouldBe` expected

--------------------------------------------------------------------------------
-- Roundtrip: encode . decode produces the same text
--------------------------------------------------------------------------------

newlineCases :: [(String, [JSONEvent], Text)]
newlineCases =
  [ ("produces nothing for an empty stream", [], ""),
    ("terminates a single scalar with a newline", [JSONNumber 1], "1\n"),
    ("places multiple scalars on separate lines", [JSONNumber 1, JSONString "a", JSONBool True, JSONNull], "1\n\"a\"\ntrue\nnull\n"),
    ("places container values on separate lines", [JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject, JSONBeginArray, JSONNumber 2, JSONEndArray], "{\"a\":1}\n[2]\n"),
    ("keeps a nested container inside one value", [JSONBeginObject, JSONObjectKey "a", JSONBeginObject, JSONObjectKey "b", JSONNumber 1, JSONEndObject, JSONEndObject, JSONNumber 2], "{\"a\":{\"b\":1}}\n2\n"),
    ("emits empty containers as single-line values", [JSONBeginObject, JSONEndObject, JSONBeginArray, JSONEndArray], "{}\n[]\n")
  ]

rawCases :: [(String, EncoderConfig, [JSONEvent], Text)]
rawCases =
  [ ("renders a top-level string without quotes or escapes", EncoderConfig Compact $ ValueOptions Raw Join, [JSONString "a\"b\n雪"], "a\"b\n雪"),
    ("keeps strings inside containers quoted", EncoderConfig Compact $ ValueOptions Raw Join, [JSONBeginArray, JSONString "x\\y", JSONEndArray], "[\"x\\\\y\"]"),
    ("keeps object keys quoted", EncoderConfig Compact $ ValueOptions Raw Join, [JSONBeginObject, JSONObjectKey "k\"", JSONString "v", JSONEndObject], "{\"k\\\"\":\"v\"}"),
    ("combines raw strings with newline separation", EncoderConfig Compact $ ValueOptions Raw NoJoin, [JSONString "one", JSONString "two", JSONNumber 3], "one\ntwo\n3\n"),
    ("combines raw strings with joining", EncoderConfig Compact $ ValueOptions Raw Join, [JSONString "one", JSONString "two", JSONNumber 3], "onetwo3")
  ]

roundtripSpec :: Spec
roundtripSpec = describe "roundtrip (encode . decode)" $ do
  forM_ roundtripCases $ \(label, input) ->
    it ("roundtrips " <> label) $ roundtrip input

roundtripCases :: [(String, Text)]
roundtripCases =
  [ ("null", "null"),
    ("true", "true"),
    ("false", "false"),
    ("number", "42"),
    ("negative number", "-3.14"),
    ("empty string", "\"\""),
    ("simple string", "\"hello\""),
    ("string with escapes", "\"hello\\nworld\""),
    ("empty object", "{}"),
    ("object with one field", "{\"name\":\"alice\"}"),
    ("object with multiple fields", "{\"a\":1,\"b\":2}"),
    ("empty array", "[]"),
    ("array with one element", "[1]"),
    ("array with multiple elements", "[1,2,3]"),
    ("array of strings", "[\"a\",\"b\",\"c\"]"),
    ("nested objects", "{\"a\":{\"b\":42}}"),
    ("nested arrays", "[[1,2],[3,4]]"),
    ("object containing array", "{\"items\":[1,2,3]}"),
    ("array containing object", "[{\"a\":1}]"),
    ("mixed types in array", "[1,\"hello\",true,null]"),
    ("deeply nested", "{\"a\":{\"b\":{\"c\":\"deep\"}}}"),
    ("complex structure", "{\"users\":[{\"name\":\"alice\",\"age\":30},{\"name\":\"bob\",\"age\":25}],\"count\":2}"),
    ("string with special escapes", "\"tab\\there\\nnewline\\rret\""),
    ("string with unicode escape", "\"\\u0041\""),
    ("number with exponent", "1e10"),
    ("number with negative exponent", "1e-2"),
    ("zero", "0")
  ]

-- | Verify that encoding with 'Pretty 2' and decoding again yields the
-- same events as decoding the compact input, i.e. that pretty-printing
-- only changes whitespace.
prettyRoundtrip :: Text -> IO ()
prettyRoundtrip input = case decodeChunks [input] of
  Left err -> expectationFailure $ "Decode failed: " <> show err
  Right events -> do
    let encoded = encodeEventsWith encConfig events
        redecoded = decodeChunks [encoded]
    case redecoded of
      Left err -> expectationFailure $ "Re-decode failed: " <> show err
      Right events' -> events `shouldBe` events'
  where
    encConfig = EncoderConfig (Pretty 2) $ ValueOptions NoRaw Join

-- | Verify that encode . decode produces the same events (wrapped in Either).
roundtrip :: Text -> IO ()
roundtrip input = case decodeChunks [input] of
  Left err -> expectationFailure $ "Decode failed: " <> show err
  Right events -> do
    let encoded = encodeEvents events
        redecoded = decodeChunks [encoded]
    case redecoded of
      Left err -> expectationFailure $ "Re-decode failed: " <> show err
      Right events' -> events `shouldBe` events'
