{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.JSON.EncoderSpec (spec) where

import qualified Data.Text as T
import HQ.JSON.Encoder
import HQ.JSON.Event (JSONEvent (..))
import Relude hiding (Compose, id)
import Streaming (Of (..))
import qualified Streaming.Prelude as S
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
encodeSpec = describe "encode" $ do
  it "encodes null"
    $ encodeEvents [JSONNull]
    `shouldBe` "null"

  it "encodes true"
    $ encodeEvents [JSONBool True]
    `shouldBe` "true"

  it "encodes false"
    $ encodeEvents [JSONBool False]
    `shouldBe` "false"

  it "encodes number"
    $ encodeEvents [JSONNumber 42]
    `shouldBe` "42"

  it "encodes string"
    $ encodeEvents [JSONString "hello"]
    `shouldBe` "\"hello\""

  it "encodes empty object"
    $ encodeEvents [JSONBeginObject, JSONEndObject]
    `shouldBe` "{}"

  it "encodes object with one field"
    $ encodeEvents
      [ JSONBeginObject,
        JSONObjectKey "name",
        JSONString "alice",
        JSONEndObject
      ]
    `shouldBe` "{\"name\":\"alice\"}"

  it "encodes object with multiple fields"
    $ encodeEvents
      [ JSONBeginObject,
        JSONObjectKey "a",
        JSONNumber 1,
        JSONObjectKey "b",
        JSONNumber 2,
        JSONEndObject
      ]
    `shouldBe` "{\"a\":1,\"b\":2}"

  it "encodes empty array"
    $ encodeEvents [JSONBeginArray, JSONEndArray]
    `shouldBe` "[]"

  it "encodes array with one element"
    $ encodeEvents [JSONBeginArray, JSONNumber 1, JSONEndArray]
    `shouldBe` "[1]"

  it "encodes array with multiple elements"
    $ encodeEvents
      [ JSONBeginArray,
        JSONNumber 1,
        JSONNumber 2,
        JSONNumber 3,
        JSONEndArray
      ]
    `shouldBe` "[1,2,3]"

  it "encodes array of strings"
    $ encodeEvents
      [ JSONBeginArray,
        JSONString "a",
        JSONString "b",
        JSONEndArray
      ]
    `shouldBe` "[\"a\",\"b\"]"

  it "encodes nested objects"
    $ encodeEvents
      [ JSONBeginObject,
        JSONObjectKey "a",
        JSONBeginObject,
        JSONObjectKey "b",
        JSONNumber 1,
        JSONEndObject,
        JSONEndObject
      ]
    `shouldBe` "{\"a\":{\"b\":1}}"

  it "encodes nested arrays"
    $ encodeEvents
      [ JSONBeginArray,
        JSONBeginArray,
        JSONNumber 1,
        JSONEndArray,
        JSONBeginArray,
        JSONNumber 2,
        JSONEndArray,
        JSONEndArray
      ]
    `shouldBe` "[[1],[2]]"

  it "encodes object containing array"
    $ encodeEvents
      [ JSONBeginObject,
        JSONObjectKey "items",
        JSONBeginArray,
        JSONNumber 1,
        JSONNumber 2,
        JSONEndArray,
        JSONEndObject
      ]
    `shouldBe` "{\"items\":[1,2]}"

  it "encodes array containing object"
    $ encodeEvents
      [ JSONBeginArray,
        JSONBeginObject,
        JSONObjectKey "a",
        JSONNumber 1,
        JSONEndObject,
        JSONEndArray
      ]
    `shouldBe` "[{\"a\":1}]"

  it "encodes complex structure"
    $ encodeEvents
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
        JSONObjectKey "count",
        JSONNumber 2,
        JSONEndObject
      ]
    `shouldBe` "{\"users\":[{\"name\":\"alice\",\"age\":30},{\"name\":\"bob\",\"age\":25}],\"count\":2}"

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
    it "empty object"
      $ encodeEventsWith prettyConfig [JSONBeginObject, JSONEndObject]
      `shouldBe` "{}"
    it "empty array"
      $ encodeEventsWith prettyConfig [JSONBeginArray, JSONEndArray]
      `shouldBe` "[]"
    it "empty containers nested in non-empty containers" $ do
      encodeEventsWith
        prettyConfig
        [ JSONBeginObject,
          JSONObjectKey "a",
          JSONBeginArray,
          JSONEndArray,
          JSONObjectKey "b",
          JSONBeginObject,
          JSONEndObject,
          JSONEndObject
        ]
        `shouldBe` "{\n  \"a\": [],\n  \"b\": {}\n}"
      encodeEventsWith
        prettyConfig
        [JSONBeginArray, JSONBeginArray, JSONEndArray, JSONBeginArray, JSONEndArray, JSONEndArray]
        `shouldBe` "[\n  [],\n  []\n]"

  describe "single-element containers" $ do
    it "object"
      $ encodeEventsWith
        prettyConfig
        [JSONBeginObject, JSONObjectKey "name", JSONString "Alice", JSONEndObject]
      `shouldBe` "{\n  \"name\": \"Alice\"\n}"
    it "array"
      $ encodeEventsWith
        prettyConfig
        [JSONBeginArray, JSONNumber 42, JSONEndArray]
      `shouldBe` "[\n  42\n]"

  describe "multiple elements" $ do
    it "object with multiple fields"
      $ encodeEventsWith
        prettyConfig
        [ JSONBeginObject,
          JSONObjectKey "a",
          JSONNumber 1,
          JSONObjectKey "b",
          JSONNumber 2,
          JSONEndObject
        ]
      `shouldBe` "{\n  \"a\": 1,\n  \"b\": 2\n}"
    it "array with multiple elements"
      $ encodeEventsWith
        prettyConfig
        [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONNumber 3, JSONEndArray]
      `shouldBe` "[\n  1,\n  2,\n  3\n]"

  describe "nested structures" $ do
    it "object in object"
      $ encodeEventsWith
        prettyConfig
        [ JSONBeginObject,
          JSONObjectKey "a",
          JSONBeginObject,
          JSONObjectKey "b",
          JSONNumber 1,
          JSONEndObject,
          JSONEndObject
        ]
      `shouldBe` "{\n  \"a\": {\n    \"b\": 1\n  }\n}"
    it "array in array"
      $ encodeEventsWith
        prettyConfig
        [JSONBeginArray, JSONBeginArray, JSONNumber 1, JSONEndArray, JSONEndArray]
      `shouldBe` "[\n  [\n    1\n  ]\n]"
    it "array in object"
      $ encodeEventsWith
        prettyConfig
        [ JSONBeginObject,
          JSONObjectKey "users",
          JSONBeginArray,
          JSONNumber 1,
          JSONNumber 2,
          JSONEndArray,
          JSONEndObject
        ]
      `shouldBe` "{\n  \"users\": [\n    1,\n    2\n  ]\n}"
    it "object in array"
      $ encodeEventsWith
        prettyConfig
        [JSONBeginArray, JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject, JSONEndArray]
      `shouldBe` "[\n  {\n    \"a\": 1\n  }\n]"
    it "deeply nested"
      $ encodeEventsWith
        prettyConfig
        [ JSONBeginObject,
          JSONObjectKey "a",
          JSONBeginObject,
          JSONObjectKey "b",
          JSONBeginArray,
          JSONBeginObject,
          JSONObjectKey "c",
          JSONNumber 1,
          JSONEndObject,
          JSONEndArray,
          JSONEndObject,
          JSONEndObject
        ]
      `shouldBe` "{\n  \"a\": {\n    \"b\": [\n      {\n        \"c\": 1\n      }\n    ]\n  }\n}"
    it "complex structure"
      $ encodeEventsWith
        prettyConfig
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
          JSONObjectKey "count",
          JSONNumber 2,
          JSONEndObject
        ]
      `shouldBe` "{\n  \"users\": [\n    {\n      \"name\": \"alice\",\n      \"age\": 30\n    },\n    {\n      \"name\": \"bob\",\n      \"age\": 25\n    }\n  ],\n  \"count\": 2\n}"
    it "mixed types in array"
      $ encodeEventsWith
        prettyConfig
        [JSONBeginArray, JSONNumber 1, JSONString "hello", JSONBool True, JSONNull, JSONEndArray]
      `shouldBe` "[\n  1,\n  \"hello\",\n  true,\n  null\n]"

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
    let roundtripCases =
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
    forM_ roundtripCases $ \(label, input) ->
      it ("pretty roundtrips " <> label) $ prettyRoundtrip input

--------------------------------------------------------------------------------
-- value output options
--------------------------------------------------------------------------------

valueOptionsSpec :: Spec
valueOptionsSpec = describe "value output options" $ do
  describe "newline separation" $ do
    let encConfig = EncoderConfig Compact $ ValueOptions NoRaw NoJoin
    it "produces nothing for an empty stream" $ do
      encodeEventsWith encConfig [] `shouldBe` ""
    it "terminates a single scalar with a newline" $ do
      encodeEventsWith encConfig [JSONNumber 1] `shouldBe` "1\n"
    it "places multiple scalars on separate lines" $ do
      encodeEventsWith
        encConfig
        [JSONNumber 1, JSONString "a", JSONBool True, JSONNull]
      `shouldBe` "1\n\"a\"\ntrue\nnull\n"

    it "places container values on separate lines" $ do
      encodeEventsWith
        encConfig
        [ JSONBeginObject,
          JSONObjectKey "a",
          JSONNumber 1,
          JSONEndObject,
          JSONBeginArray,
          JSONNumber 2,
          JSONEndArray
        ]
      `shouldBe` "{\"a\":1}\n[2]\n"
    it "keeps a nested container inside one value" $ do
      encodeEventsWith
        encConfig
        [ JSONBeginObject,
          JSONObjectKey "a",
          JSONBeginObject,
          JSONObjectKey "b",
          JSONNumber 1,
          JSONEndObject,
          JSONEndObject,
          JSONNumber 2
        ]
      `shouldBe` "{\"a\":{\"b\":1}}\n2\n"
    it "emits empty containers as single-line values" $ do
      encodeEventsWith
        encConfig
        [JSONBeginObject, JSONEndObject, JSONBeginArray, JSONEndArray]
      `shouldBe` "{}\n[]\n"
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

  describe "raw string output" $ do
    let encConfig = EncoderConfig Compact $ ValueOptions Raw Join
    it "renders a top-level string without quotes or escapes" $ do
      encodeEventsWith encConfig [JSONString "a\"b\n雪"] `shouldBe` "a\"b\n雪"
    it "keeps strings inside containers quoted" $ do
      encodeEventsWith
        encConfig
        [JSONBeginArray, JSONString "x\\y", JSONEndArray]
      `shouldBe` "[\"x\\\\y\"]"
    it "keeps object keys quoted" $ do
      encodeEventsWith
        encConfig
        [JSONBeginObject, JSONObjectKey "k\"", JSONString "v", JSONEndObject]
      `shouldBe` "{\"k\\\"\":\"v\"}"
    it "combines raw strings with newline separation" $ do
      encodeEventsWith
        (EncoderConfig Compact $ ValueOptions Raw NoJoin)
        [JSONString "one", JSONString "two", JSONNumber 3]
      `shouldBe` "one\ntwo\n3\n"
    it "combines raw strings with joining" $ do
      encodeEventsWith
        encConfig
        [JSONString "one", JSONString "two", JSONNumber 3]
      `shouldBe` "onetwo3"

--------------------------------------------------------------------------------
-- Roundtrip: encode . decode produces the same text
--------------------------------------------------------------------------------

roundtripSpec :: Spec
roundtripSpec = describe "roundtrip (encode . decode)" $ do
  it "roundtrips null" $ roundtrip "null"
  it "roundtrips true" $ roundtrip "true"
  it "roundtrips false" $ roundtrip "false"
  it "roundtrips number" $ roundtrip "42"
  it "roundtrips negative number" $ roundtrip "-3.14"
  it "roundtrips empty string" $ roundtrip "\"\""
  it "roundtrips simple string" $ roundtrip "\"hello\""
  it "roundtrips string with escapes" $ roundtrip "\"hello\\nworld\""
  it "roundtrips empty object" $ roundtrip "{}"
  it "roundtrips object with one field" $ roundtrip "{\"name\":\"alice\"}"
  it "roundtrips object with multiple fields" $ roundtrip "{\"a\":1,\"b\":2}"
  it "roundtrips empty array" $ roundtrip "[]"
  it "roundtrips array with one element" $ roundtrip "[1]"
  it "roundtrips array with multiple elements" $ roundtrip "[1,2,3]"
  it "roundtrips array of strings" $ roundtrip "[\"a\",\"b\",\"c\"]"
  it "roundtrips nested objects" $ roundtrip "{\"a\":{\"b\":42}}"
  it "roundtrips nested arrays" $ roundtrip "[[1,2],[3,4]]"
  it "roundtrips object containing array" $ roundtrip "{\"items\":[1,2,3]}"
  it "roundtrips array containing object" $ roundtrip "[{\"a\":1}]"
  it "roundtrips mixed types in array" $ roundtrip "[1,\"hello\",true,null]"
  it "roundtrips deeply nested" $ roundtrip "{\"a\":{\"b\":{\"c\":\"deep\"}}}"
  it "roundtrips complex structure" $ roundtrip "{\"users\":[{\"name\":\"alice\",\"age\":30},{\"name\":\"bob\",\"age\":25}],\"count\":2}"
  it "roundtrips string with special escapes" $ roundtrip "\"tab\\there\\nnewline\\rret\""
  it "roundtrips string with unicode escape" $ roundtrip "\"\\u0041\""
  it "roundtrips number with exponent" $ roundtrip "1e10"
  it "roundtrips number with negative exponent" $ roundtrip "1e-2"
  it "roundtrips zero" $ roundtrip "0"

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
