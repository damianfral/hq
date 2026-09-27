{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.JSON.DecoderSpec (spec) where

import Data.Scientific (fromFloatDigits)
import HQ.JSON.Decoder
import HQ.JSON.Event
import Relude hiding (Compose, id)
import qualified Streaming.Prelude as S
import Test.HQ (decodeChunks, decodeShown, drainCollect, malformedPullCorpus, runStreaming, splits, validPullCorpus)
import Test.Syd

spec :: Spec
spec = describe "HQ.JSON.Decoder" $ do
  scalarSpec
  stringSpec
  numberSpec
  objectSpec
  arraySpec
  nestedSpec
  incrementalSpec
  errorSpec
  streamingSpec
  adversarialSpec
  pullEventSpec

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Decode input that is guaranteed to be complete (all at once).
decodeComplete :: Text -> Either DecodeError [JSONEvent]
decodeComplete input = case feed input initialDecoder of
  Left err -> Left err
  Right result -> drainCollect result

-- | Decode text incrementally, feeding one character at a time.
decodeIncremental :: Text -> Either DecodeError [JSONEvent]
decodeIncremental input = go initialDecoder (toString input)
  where
    -- After all characters fed, drain remaining events and finalize
    go decoder [] = case step decoder of
      Left err -> Left err
      Right result -> drainCollect result
    go decoder (c : cs) = case feed (one c) decoder of
      Left err -> Left err
      Right (Emit event nextDecoder) -> fmap (event :) (go nextDecoder cs)
      Right (NeedInput nextDecoder) -> go nextDecoder cs
      Right (Done nextDecoder) -> go nextDecoder cs

--------------------------------------------------------------------------------
-- Scalar values
--------------------------------------------------------------------------------

scalarSpec :: Spec
scalarSpec = describe "scalars" $ do
  describe "null" $ do
    it "parses null" $ decodeComplete "null" `shouldBe` Right [JSONNull]

  describe "booleans" $ do
    it "parses true" $ decodeComplete "true" `shouldBe` Right [JSONBool True]
    it "parses false" $ decodeComplete "false" `shouldBe` Right [JSONBool False]

  describe "numbers" $ do
    it "parses zero" $ decodeComplete "0" `shouldBe` Right [JSONNumber 0]

    it "parses positive integer" $ do
      decodeComplete "42" `shouldBe` Right [JSONNumber 42]

    it "parses negative integer" $ do
      decodeComplete "-42" `shouldBe` Right [JSONNumber (-42)]

    it "parses decimal" $ do
      decodeComplete "3.14"
        `shouldBe` Right [JSONNumber (fromFloatDigits (3.14 :: Double))]

    it "parses negative decimal" $ do
      decodeComplete "-3.14"
        `shouldBe` Right [JSONNumber (fromFloatDigits (-3.14 :: Double))]

    it "parses scientific notation" $ do
      decodeComplete "1e10" `shouldBe` Right [JSONNumber 10000000000]

    it "parses scientific notation with exponent" $ do
      decodeComplete "1.5e2" `shouldBe` Right [JSONNumber 150]

    it "parses negative mantissa with unsigned exponent" $ do
      decodeComplete "-1e5" `shouldBe` Right [JSONNumber (-100000)]

    it "parses negative decimal with exponent" $ do
      decodeComplete "-1.5e2" `shouldBe` Right [JSONNumber (-150)]

  describe "strings" $ do
    it "parses empty string" $ do
      decodeComplete "\"\"" `shouldBe` Right [JSONString ""]

    it "parses simple string" $ do
      decodeComplete "\"hello\"" `shouldBe` Right [JSONString "hello"]

    it "parses string with spaces" $ do
      decodeComplete "\"hello world\""
        `shouldBe` Right [JSONString "hello world"]

--------------------------------------------------------------------------------
-- String escaping
--------------------------------------------------------------------------------

stringSpec :: Spec
stringSpec = describe "string escape sequences" $ do
  it "parses escaped quote" $ do
    decodeComplete "\"hello\\\"world\""
      `shouldBe` Right [JSONString "hello\"world"]

  it "parses escaped backslash" $ do
    decodeComplete "\"hello\\\\world\""
      `shouldBe` Right [JSONString "hello\\world"]

  it "parses escaped newline" $ do
    decodeComplete "\"hello\\nworld\""
      `shouldBe` Right [JSONString "hello\nworld"]

  it "parses escaped tab" $ do
    decodeComplete "\"hello\\tworld\""
      `shouldBe` Right [JSONString "hello\tworld"]

  it "parses escaped carriage return" $ do
    decodeComplete "\"hello\\rworld\""
      `shouldBe` Right [JSONString "hello\rworld"]

  it "parses escaped backspace" $ do
    decodeComplete "\"hello\\bworld\""
      `shouldBe` Right [JSONString "hello\bworld"]

  it "parses escaped form feed" $ do
    decodeComplete "\"hello\\fworld\""
      `shouldBe` Right [JSONString "hello\fworld"]

  it "parses escaped forward slash" $ do
    decodeComplete "\"hello\\/world\""
      `shouldBe` Right [JSONString "hello/world"]

  it "parses unicode escape" $ do
    decodeComplete "\"\\u0041\"" `shouldBe` Right [JSONString "A"]

  it "parses multi-byte unicode escape" $ do
    decodeComplete "\"\\u00E9\"" `shouldBe` Right [JSONString "\233"]

  it "parses surrogate pair" $ do
    -- U+1D11E (Musical Symbol G Clef) = D834 DD1E
    decodeComplete "\"\\uD834\\uDD1E\"" `shouldBe` Right [JSONString "\119070"]

  it "does not consume a hex digit following a unicode escape" $ do
    decodeComplete "\"\\u0026B\"" `shouldBe` Right [JSONString "&B"]
    decodeComplete "\"\\u0041B\"" `shouldBe` Right [JSONString "AB"]

  it "parses string with multiple escapes" $ do
    decodeComplete "\"line1\\nline2\\ttab\""
      `shouldBe` Right [JSONString "line1\nline2\ttab"]

  it "rejects invalid escape character" $ do
    case decodeComplete "\"hello\\xworld\"" of
      Left (InvalidEscape 'x') -> pure ()
      other ->
        expectationFailure $ "Expected InvalidEscape, got: " <> show other

--------------------------------------------------------------------------------
-- Numbers
--------------------------------------------------------------------------------

numberSpec :: Spec
numberSpec = describe "number formats" $ do
  it "parses single digit" $ do
    decodeComplete "5" `shouldBe` Right [JSONNumber 5]

  it "parses multiple digits" $ do
    decodeComplete "123" `shouldBe` Right [JSONNumber 123]

  it "parses leading zero" $ do
    decodeComplete "0" `shouldBe` Right [JSONNumber 0]

  it "parses negative zero" $ do
    decodeComplete "-0" `shouldBe` Right [JSONNumber 0]

  it "parses with fractional part" $ do
    decodeComplete "1.0" `shouldBe` Right [JSONNumber 1]

  it "parses with exponent E" $ do
    decodeComplete "1E10" `shouldBe` Right [JSONNumber 10000000000]

  it "parses with exponent e" $ do
    decodeComplete "1e10" `shouldBe` Right [JSONNumber 10000000000]

  it "parses with negative exponent" $ do
    decodeComplete "1e-2"
      `shouldBe` Right [JSONNumber (fromFloatDigits (0.01 :: Double))]

  it "parses with positive exponent sign" $ do
    decodeComplete "1e+2" `shouldBe` Right [JSONNumber 100]

  it "parses large number" $ do
    decodeComplete "99999999999999999999"
      `shouldBe` Right [JSONNumber 99999999999999999999]

  it "parses -1.5e-6" $ do
    decodeComplete "-1.5e-6"
      `shouldBe` Right [JSONNumber (fromFloatDigits ((-1.5e-6) :: Double))]

  it "parses 1.25" $ do
    decodeComplete "1.25"
      `shouldBe` Right [JSONNumber (fromFloatDigits (1.25 :: Double))]

  it "parses 1E+10" $ do
    decodeComplete "1E+10" `shouldBe` Right [JSONNumber 10000000000]

--------------------------------------------------------------------------------
-- Objects
--------------------------------------------------------------------------------

objectSpec :: Spec
objectSpec = describe "objects" $ do
  it "parses empty object" $ do
    decodeComplete "{}"
      `shouldBe` Right [JSONBeginObject, JSONEndObject]

  it "parses object with one field" $ do
    let expected =
          [ JSONBeginObject,
            JSONObjectKey "name",
            JSONString "alice",
            JSONEndObject
          ]
    decodeComplete "{\"name\":\"alice\"}" `shouldBe` Right expected

  it "parses object with multiple fields" $ do
    let expected =
          [ JSONBeginObject,
            JSONObjectKey "a",
            JSONNumber 1,
            JSONObjectKey "b",
            JSONNumber 2,
            JSONEndObject
          ]
    decodeComplete "{\"a\":1,\"b\":2}" `shouldBe` Right expected

  it "parses object with string value" $ do
    let expected =
          [ JSONBeginObject,
            JSONObjectKey "key",
            JSONString "value",
            JSONEndObject
          ]
    decodeComplete "{\"key\":\"value\"}" `shouldBe` Right expected

  it "parses object with null value" $ do
    let expected = [JSONBeginObject, JSONObjectKey "x", JSONNull, JSONEndObject]
    decodeComplete "{\"x\":null}" `shouldBe` Right expected

  it "parses object with boolean value" $ do
    let expected =
          [JSONBeginObject, JSONObjectKey "flag", JSONBool True, JSONEndObject]
    decodeComplete "{\"flag\":true}" `shouldBe` Right expected

  it "parses object with number value" $ do
    let expected =
          [JSONBeginObject, JSONObjectKey "count", JSONNumber 42, JSONEndObject]
    decodeComplete "{\"count\":42}" `shouldBe` Right expected

  it "parses object with whitespace" $ do
    let expected =
          [ JSONBeginObject,
            JSONObjectKey "name",
            JSONString "alice",
            JSONEndObject
          ]
    decodeComplete "{ \"name\" : \"alice\" }" `shouldBe` Right expected

--------------------------------------------------------------------------------
-- Arrays
--------------------------------------------------------------------------------

arraySpec :: Spec
arraySpec = describe "arrays" $ do
  it "parses empty array" $ do
    decodeComplete "[]" `shouldBe` Right [JSONBeginArray, JSONEndArray]

  it "parses array with one element" $ do
    decodeComplete "[1]"
      `shouldBe` Right [JSONBeginArray, JSONNumber 1, JSONEndArray]

  it "parses array with multiple elements" $ do
    let expected =
          [ JSONBeginArray,
            JSONNumber 1,
            JSONNumber 2,
            JSONNumber 3,
            JSONEndArray
          ]
    decodeComplete "[1,2,3]" `shouldBe` Right expected

  it "parses array with mixed types" $ do
    let expected =
          [ JSONBeginArray,
            JSONNumber 1,
            JSONString "hello",
            JSONBool True,
            JSONNull,
            JSONEndArray
          ]
    decodeComplete "[1,\"hello\",true,null]" `shouldBe` Right expected

  it "parses array with whitespace" $ do
    let expected =
          [ JSONBeginArray,
            JSONNumber 1,
            JSONNumber 2,
            JSONNumber 3,
            JSONEndArray
          ]
    decodeComplete "[ 1 , 2 , 3 ]" `shouldBe` Right expected

  it "parses array of strings" $ do
    let expected =
          [ JSONBeginArray,
            JSONString "a",
            JSONString "b",
            JSONString "c",
            JSONEndArray
          ]
    decodeComplete "[\"a\",\"b\",\"c\"]" `shouldBe` Right expected

  it "parses array of booleans" $ do
    let expected =
          [ JSONBeginArray,
            JSONBool True,
            JSONBool False,
            JSONBool True,
            JSONEndArray
          ]
    decodeComplete "[true,false,true]" `shouldBe` Right expected

--------------------------------------------------------------------------------
-- Nested structures
--------------------------------------------------------------------------------

nestedSpec :: Spec
nestedSpec = describe "nested structures" $ do
  it "parses nested objects" $ do
    decodeComplete "{\"a\":{\"b\":42}}"
      `shouldBe` Right
        [ JSONBeginObject,
          JSONObjectKey "a",
          JSONBeginObject,
          JSONObjectKey "b",
          JSONNumber 42,
          JSONEndObject,
          JSONEndObject
        ]

  it "parses nested arrays" $ do
    decodeComplete "[[1,2],[3,4]]"
      `shouldBe` Right
        [ JSONBeginArray,
          JSONBeginArray,
          JSONNumber 1,
          JSONNumber 2,
          JSONEndArray,
          JSONBeginArray,
          JSONNumber 3,
          JSONNumber 4,
          JSONEndArray,
          JSONEndArray
        ]

  it "parses object containing array" $ do
    decodeComplete "{\"items\":[1,2,3]}"
      `shouldBe` Right
        [ JSONBeginObject,
          JSONObjectKey "items",
          JSONBeginArray,
          JSONNumber 1,
          JSONNumber 2,
          JSONNumber 3,
          JSONEndArray,
          JSONEndObject
        ]

  it "parses array containing object" $ do
    decodeComplete "[{\"a\":1}]"
      `shouldBe` Right
        [ JSONBeginArray,
          JSONBeginObject,
          JSONObjectKey "a",
          JSONNumber 1,
          JSONEndObject,
          JSONEndArray
        ]

  it "parses deeply nested structure" $ do
    decodeComplete "{\"a\":{\"b\":{\"c\":\"deep\"}}}"
      `shouldBe` Right
        [ JSONBeginObject,
          JSONObjectKey "a",
          JSONBeginObject,
          JSONObjectKey "b",
          JSONBeginObject,
          JSONObjectKey "c",
          JSONString "deep",
          JSONEndObject,
          JSONEndObject,
          JSONEndObject
        ]

  it "parses complex real-world-like structure" $ do
    let input = "{\"users\":[{\"name\":\"alice\",\"age\":30},{\"name\":\"bob\",\"age\":25}],\"count\":2}"
    case decodeComplete input of
      Left err -> expectationFailure $ "Parse error: " <> show err
      Right events -> do
        viaNonEmpty head events `shouldBe` Just JSONBeginObject
        length events `shouldSatisfy` (> 10)

--------------------------------------------------------------------------------
-- Incremental parsing
--------------------------------------------------------------------------------

incrementalSpec :: Spec
incrementalSpec = describe "incremental parsing" $ do
  it "parses scalar incrementally" $ do
    decodeIncremental "42" `shouldBe` Right [JSONNumber 42]

  it "parses string incrementally" $ do
    decodeIncremental "\"hi\"" `shouldBe` Right [JSONString "hi"]

  it "parses object incrementally" $ do
    let input = "{\"a\":1}"
    case decodeIncremental input of
      Left err -> expectationFailure $ "Parse error: " <> show err
      Right events -> do
        viaNonEmpty head events `shouldBe` Just JSONBeginObject
        viaNonEmpty last events `shouldBe` Just JSONEndObject

  it "parses array incrementally" $ do
    let input = "[1,2,3]"
    let expected =
          [ JSONBeginArray,
            JSONNumber 1,
            JSONNumber 2,
            JSONNumber 3,
            JSONEndArray
          ]
    case decodeIncremental input of
      Left err -> expectationFailure $ "Parse error: " <> show err
      Right events -> events `shouldBe` expected

  it "parses nested incrementally" $ do
    let input = "[[1],[2]]"
    case decodeIncremental input of
      Left err -> expectationFailure $ "Parse error: " <> show err
      Right events -> length events `shouldSatisfy` (>= 8)

  it "parses number incrementally across chunk boundary" $ do
    -- Number split across chunks: "4" then "2"
    case decodeChunks ["4", "2"] of
      Left err -> expectationFailure $ "Parse error: " <> show err
      Right events -> events `shouldBe` [JSONNumber 42]

  it "parses object across 3 chunks" $ do
    let expected =
          [JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject]
    case decodeChunks ["{\"a\":", "1", "}"] of
      Left err -> expectationFailure $ "Parse error: " <> show err
      Right events -> events `shouldBe` expected

--------------------------------------------------------------------------------
-- Error handling
--------------------------------------------------------------------------------

errorSpec :: Spec
errorSpec = describe "error handling" $ do
  it "rejects empty input" $ case decodeComplete "" of
    Left UnexpectedEnd -> pure ()
    other -> expectationFailure $ "Expected UnexpectedEnd, got: " <> show other

  it "rejects incomplete object" $ case decodeComplete "{\"a\":" of
    Left UnexpectedEnd -> pure ()
    other -> expectationFailure $ "Expected UnexpectedEnd, got: " <> show other

  it "rejects trailing input" $ case decodeComplete "42 extra" of
    Left TrailingInput -> pure ()
    other -> expectationFailure $ "Expected TrailingInput, got: " <> show other

  it "rejects trailing input after object" $ case decodeComplete "{} {}" of
    Left TrailingInput -> pure ()
    other -> expectationFailure $ "Expected TrailingInput, got: " <> show other

  it "rejects invalid keyword" $ case decodeComplete "nul" of
    Left (InvalidKeyword _) -> pure ()
    Left UnexpectedEnd -> pure ()
    other -> expectationFailure $ "Expected error, got: " <> show other

  it "rejects invalid character" $ case decodeComplete "x" of
    Left (UnexpectedChar 'x') -> pure ()
    other -> expectationFailure $ "Expected UnexpectedChar, got: " <> show other

  it "rejects unmatched opening bracket" $ case decodeComplete "[" of
    Left UnexpectedEnd -> pure ()
    other -> expectationFailure $ "Expected UnexpectedEnd, got: " <> show other

  it "rejects unmatched closing bracket" $ case decodeComplete "]" of
    Left (UnexpectedChar ']') -> pure ()
    Left UnexpectedEnd -> pure ()
    other -> expectationFailure $ "Expected error, got: " <> show other

  it "rejects missing colon in object" $ case decodeComplete "{\"a\" 1}" of
    Left ExpectedColon -> pure ()
    other -> expectationFailure $ "Expected ExpectedColon, got: " <> show other

  it "rejects missing comma in array" $ case decodeComplete "[1 2]" of
    Left ExpectedCommaOrEnd -> pure ()
    other ->
      expectationFailure $ "Expected ExpectedCommaOrEnd, got: " <> show other

  it "rejects invalid escape in string" $ case decodeComplete "\"\\z\"" of
    Left (InvalidEscape 'z') -> pure ()
    other -> expectationFailure $ "Expected InvalidEscape, got: " <> show other

  it "rejects invalid unicode escape" $ case decodeComplete "\"\\uGGGG\"" of
    Left InvalidUnicodeEscape -> pure ()
    other ->
      expectationFailure $ "Expected InvalidUnicodeEscape, got: " <> show other

  it "rejects invalid number --" $ case decodeComplete "--" of
    Left (InvalidNumber _) -> pure ()
    Left (UnexpectedChar '-') -> pure ()
    other -> expectationFailure $ "Expected error, got: " <> show other

--------------------------------------------------------------------------------
-- Streaming adapter
--------------------------------------------------------------------------------

streamingSpec :: Spec
streamingSpec = describe "streaming decode" $ do
  it "decodes a single chunk" $ runStreaming ["42"] `shouldBe` [JSONNumber 42]

  it "decodes multiple chunks" $ do
    let expected =
          [JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject]
    runStreaming ["{", "\"a\"", ":", "1", "}"] `shouldBe` expected

  it "decodes empty stream" $ runStreaming [] `shouldBe` []

  it "decodes scalar values" $ runStreaming ["null"] `shouldBe` [JSONNull]

  it "decodes boolean" $ runStreaming ["true"] `shouldBe` [JSONBool True]

  it "decodes string across chunks" $ do
    runStreaming ["\"hel", "lo\""] `shouldBe` [JSONString "hello"]

  it "decodes array across chunks" $ do
    let expected =
          [ JSONBeginArray,
            JSONNumber 1,
            JSONNumber 2,
            JSONNumber 3,
            JSONEndArray
          ]
    runStreaming ["[1,", "2,", "3]"] `shouldBe` expected

  it "decodes object across chunks" $ do
    let expected =
          [JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject]
    runStreaming ["{\"a\":", "1}"] `shouldBe` expected

  it "decodes nested structure across chunks" $ do
    let expected =
          [ JSONBeginArray,
            JSONBeginArray,
            JSONNumber 1,
            JSONEndArray,
            JSONBeginArray,
            JSONNumber 2,
            JSONEndArray,
            JSONEndArray
          ]
    runStreaming ["[[1],", "[2]]"] `shouldBe` expected

  it "reports error on invalid input" $ case decodeChunks ["{invalid}"] of
    Left (UnexpectedChar 'i') -> pure ()
    Left _ -> pure ()
    Right events ->
      expectationFailure $ "Expected parse error, got events: " <> show events

  it "reports error on invalid character from non-empty stream" $ do
    case decodeChunks ["{", "invalid}"] of
      Left _ -> pure ()
      Right events ->
        expectationFailure $ "Expected parse error, got events: " <> show events

  it "decodes single-character chunks" $ do
    let expected = [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONEndArray]
    runStreaming ["[", "1", ",", "2", "]"] `shouldBe` expected

  it "decodes string literal across 5 chunks" $ do
    let expected = [JSONString "hello world"]
    runStreaming ["\"", "he", "llo", " wor", "ld\""] `shouldBe` expected

  it "decodes complex nested across many chunks" $ do
    let expected =
          [ JSONBeginObject,
            JSONObjectKey "arr",
            JSONBeginArray,
            JSONNumber 1,
            JSONNumber 2,
            JSONEndArray,
            JSONObjectKey "key",
            JSONString "val",
            JSONEndObject
          ]
    runStreaming ["{\"arr\":[", "1,2],", "\"key\":\"val\"}"] `shouldBe` expected

  it "parses surrogate pair across chunks" $ do
    runStreaming ["\"\\uD834", "\\uDD1E\""] `shouldBe` [JSONString "\119070"]

  it "parses unicode escape split across chunks followed by hex digit" $ do
    runStreaming ["\"\\u004", "1B\""] `shouldBe` [JSONString "AB"]

  it "parses object with unicode value" $ do
    let expected =
          [JSONBeginObject, JSONObjectKey "k", JSONString "A", JSONEndObject]
    runStreaming ["{\"k\":\"\\u0041\"}"] `shouldBe` expected

  it "parses nested arrays across many chunks" $ do
    let expected =
          [ JSONBeginArray,
            JSONBeginArray,
            JSONBeginArray,
            JSONNumber 1,
            JSONEndArray,
            JSONEndArray,
            JSONEndArray
          ]
    runStreaming ["[[", "[", "1", "]", "]]"] `shouldBe` expected

  it "parses escape sequences at chunk boundary" $ do
    runStreaming ["\"\\", "n\""] `shouldBe` [JSONString "\n"]

  it "reports trailing input from non-empty stream" $ do
    case decodeChunks ["42", " ", "extra"] of
      Left TrailingInput -> pure ()
      Left _ -> pure ()
      Right events ->
        expectationFailure $ "Expected TrailingInput, got: " <> show events

  it "reports error on empty chunk with trailing data"
    $ case decodeChunks ["42", "extra"] of
      Left TrailingInput -> pure ()
      Left _ -> pure ()
      Right events -> expectationFailure $ "Expected error, got events: " <> show events

  it "parses deeply nested object across chunks" $ do
    let input = ["{\"a\":{\"b\":", "{\"", "c", "\":", "42}}}"]
    let expected =
          [ JSONBeginObject,
            JSONObjectKey "a",
            JSONBeginObject,
            JSONObjectKey "b",
            JSONBeginObject,
            JSONObjectKey "c",
            JSONNumber 42,
            JSONEndObject,
            JSONEndObject,
            JSONEndObject
          ]
    runStreaming input `shouldBe` expected

  it "parses number 0 across chunks" $ do
    runStreaming ["0"] `shouldBe` [JSONNumber 0]

  it "parses number -42 across chunks" $ do
    runStreaming ["-4", "2"] `shouldBe` [JSONNumber (-42)]

  it "parses fractional number across chunks" $ do
    let expected = [JSONNumber (fromFloatDigits (3.14 :: Double))]
    runStreaming ["3.", "14"] `shouldBe` expected

  it "parses exponent number across chunks" $ do
    runStreaming ["1e", "10"] `shouldBe` [JSONNumber 10000000000]

--------------------------------------------------------------------------------
-- Adversarial tests: invalid numbers, surrogate pairs, error propagation
--------------------------------------------------------------------------------

adversarialSpec :: Spec
adversarialSpec = describe "malformed input" $ do
  -- ---- Invalid numbers ----
  describe "invalid numbers" $ do
    it "rejects +1 (leading plus)" $ case decodeComplete "+1" of
      Left (UnexpectedChar '+') -> pure ()
      Left (InvalidNumber _) -> pure ()
      other -> expectationFailure $ "Expected error for +1, got: " <> show other

    it "rejects 01 (leading zero)" $ case decodeComplete "01" of
      Left (InvalidNumber _) -> pure ()
      other ->
        expectationFailure
          ("Expected InvalidNumber for 01, got: " <> show other)

    it "rejects 1. (trailing dot)" $ case decodeComplete "1." of
      Left (InvalidNumber _) -> pure ()
      Left UnexpectedEnd -> pure ()
      other ->
        expectationFailure
          $ "Expected InvalidNumber or UnexpectedEnd for 1., got: "
          <> show other

    it "rejects 1e (trailing exponent)" $ case decodeComplete "1e" of
      Left (InvalidNumber _) -> pure ()
      Left UnexpectedEnd -> pure ()
      other ->
        expectationFailure
          $ "Expected InvalidNumber or UnexpectedEnd for 1e, got: "
          <> show other

    it "rejects 1e+ (trailing exponent sign)" $ case decodeComplete "1e+" of
      Left (InvalidNumber _) -> pure ()
      Left UnexpectedEnd -> pure ()
      other ->
        expectationFailure
          $ "Expected InvalidNumber or UnexpectedEnd for 1e+, got: "
          <> show other

    it "rejects - (just a sign)" $ case decodeComplete "-" of
      Left (InvalidNumber _) -> pure ()
      Left UnexpectedEnd -> pure ()
      other -> expectationFailure $ "Expected error for -, got: " <> show other

    it "rejects 1--2 (double negative)" $ case decodeComplete "1--2" of
      Left (InvalidNumber _) -> pure ()
      Left (UnexpectedChar '-') -> pure ()
      other ->
        expectationFailure $ "Expected error for 1--2, got: " <> show other

    it "rejects 1.2.3 (double dot)" $ case decodeComplete "1.2.3" of
      Left (InvalidNumber _) -> pure ()
      Left (UnexpectedChar '.') -> pure ()
      other ->
        expectationFailure $ "Expected error for 1.2.3, got: " <> show other

    it "rejects -. (sign + dot only)" $ case decodeComplete "-." of
      Left (InvalidNumber _) -> pure ()
      Left (UnexpectedChar '.') -> pure ()
      other -> expectationFailure $ "Expected error for -., got: " <> show other

    it "rejects 1e--2 (double exponent sign)" $ case decodeComplete "1e--2" of
      Left (InvalidNumber _) -> pure ()
      Left (UnexpectedChar '-') -> pure ()
      other ->
        expectationFailure $ "Expected error for 1e--2, got: " <> show other

  -- ---- Surrogate pair errors at EOF ----
  describe "surrogate pair errors" $ do
    it "rejects incomplete high surrogate at EOF" $ do
      case decodeComplete "\"\\uD834" of
        Left InvalidSurrogatePair -> pure ()
        Left UnexpectedEnd -> pure ()
        other ->
          expectationFailure
            $ "Expected InvalidSurrogatePair or UnexpectedEnd for incomplete high surrogate, got: "
            <> show other

    it "rejects high surrogate followed by non-low-surrogate" $ do
      case decodeComplete "\"\\uD834\\u0041\"" of
        Left InvalidSurrogatePair -> pure ()
        other ->
          expectationFailure
            $ "Expected InvalidSurrogatePair, got: "
            <> show other

    it "rejects lone low surrogate" $ case decodeComplete "\"\\uDC00\"" of
      Left InvalidSurrogatePair -> pure ()
      other ->
        expectationFailure $ "Expected InvalidSurrogatePair, got: " <> show other

    it "rejects incomplete low surrogate escape at EOF" $ do
      case decodeComplete "\"\\uD834\\u" of
        Left InvalidSurrogatePair -> pure ()
        Left InvalidUnicodeEscape -> pure ()
        Left UnexpectedEnd -> pure ()
        other ->
          expectationFailure
            $ "Expected error for incomplete low surrogate, got: "
            <> show other

    it "rejects high surrogate followed by invalid unicode" $ do
      case decodeComplete "\"\\uD834\\uGGGG\"" of
        Left InvalidSurrogatePair -> pure ()
        Left InvalidUnicodeEscape -> pure ()
        other ->
          expectationFailure
            $ "Expected InvalidSurrogatePair or InvalidUnicodeEscape, got: "
            <> show other

  -- ---- Error propagation in streaming (the key continueWith bug) ----
  describe "error propagation" $ do
    it "propagates error from streaming invalid input"
      $ case decodeChunks ["{\"foo\": @}}"] of
        Left (UnexpectedChar '@') -> pure ()
        Left _ -> pure ()
        Right events -> expectationFailure $ "Expected parse error, got: " <> show events

    it "propagates error after key in streaming" $ do
      case decodeChunks ["{\"a\":", "@}"] of
        Left (UnexpectedChar '@') -> pure ()
        Left _ -> pure ()
        Right events ->
          expectationFailure $ "Expected parse error, got: " <> show events

    it "propagates error after colon in streaming" $ do
      case decodeChunks ["{\"a\":@", "}"] of
        Left (UnexpectedChar '@') -> pure ()
        Left _ -> pure ()
        Right events ->
          expectationFailure $ "Expected parse error, got: " <> show events

    it "propagates error in deeply nested streaming" $ do
      case decodeChunks ["[[[", "@]]]"] of
        Left (UnexpectedChar '@') -> pure ()
        Left _ -> pure ()
        Right events ->
          expectationFailure $ "Expected parse error, got: " <> show events

    it "propagates error after array element in streaming" $ do
      case decodeChunks ["[1,", "@2]"] of
        Left (UnexpectedChar '@') -> pure ()
        Left _ -> pure ()
        Right events ->
          expectationFailure $ "Expected parse error, got: " <> show events

    it "parses keyword true split across chunks" $ do
      case decodeChunks ["tru", "e"] of
        Right [JSONBool True] -> pure () -- "true" split across chunks is valid
        other ->
          expectationFailure $ "Expected [JSONBool True], got: " <> show other

    it "propagates string escape error at chunk boundary" $ do
      case decodeChunks ["\"\\", "z\""] of
        Left (InvalidEscape 'z') -> pure ()
        Left _ -> pure ()
        Right events ->
          expectationFailure $ "Expected InvalidEscape, got: " <> show events

  -- ---- Empty and boundary ----
  describe "edge cases" $ do
    it "single whitespace stream is empty" $ runStreaming [" "] `shouldBe` []

    it "multiple empty chunks then data" $ do
      runStreaming ["", "", "42"] `shouldBe` [JSONNumber 42]

    it "data then empty chunks" $ do
      runStreaming ["42", "", ""] `shouldBe` [JSONNumber 42]

    it "deeply nested empty arrays" $ do
      let expected =
            [ JSONBeginArray,
              JSONBeginArray,
              JSONBeginArray,
              JSONBeginArray,
              JSONEndArray,
              JSONEndArray,
              JSONEndArray,
              JSONEndArray
            ]
      decodeComplete "[[[[]]]]" `shouldBe` Right expected

    it "all scalar types in array" $ do
      let expected =
            [ JSONBeginArray,
              JSONNull,
              JSONBool True,
              JSONBool False,
              JSONNumber 0,
              JSONNumber (fromFloatDigits (1.5 :: Double)),
              JSONString "x",
              JSONEndArray
            ]
      decodeComplete "[null,true,false,0,1.5,\"x\"]" `shouldBe` Right expected

    it "empty object value" $ do
      let expected =
            [ JSONBeginObject,
              JSONObjectKey "a",
              JSONBeginObject,
              JSONEndObject,
              JSONEndObject
            ]
      decodeComplete "{\"a\":{}}" `shouldBe` Right expected

    it "empty array value" $ do
      let expected =
            [ JSONBeginObject,
              JSONObjectKey "a",
              JSONBeginArray,
              JSONEndArray,
              JSONEndObject
            ]
      decodeComplete "{\"a\":[]}" `shouldBe` Right expected

    it "unterminated string" $ case decodeComplete "\"hello" of
      Left UnexpectedEnd -> pure ()
      other -> expectationFailure $ "Expected UnexpectedEnd, got: " <> show other

    it "unterminated string with escape" $ case decodeComplete "\"hello\\" of
      Left UnexpectedEnd -> pure ()
      other -> expectationFailure $ "Expected UnexpectedEnd, got: " <> show other

    it "unterminated unicode escape" $ case decodeComplete "\"\\u00" of
      Left UnexpectedEnd -> pure ()
      other -> expectationFailure $ "Expected UnexpectedEnd, got: " <> show other

    it "unterminated object" $ case decodeComplete "{\"a\":1" of
      Left UnexpectedEnd -> pure ()
      other -> expectationFailure $ "Expected UnexpectedEnd, got: " <> show other

    it "unterminated array" $ case decodeComplete "[1,2" of
      Left UnexpectedEnd -> pure ()
      other -> expectationFailure $ "Expected UnexpectedEnd, got: " <> show other

    it "keyword null without delimiter at EOF" $ do
      decodeComplete "null" `shouldBe` Right [JSONNull]

    it "keyword null followed by digit is invalid" $ do
      case decodeComplete "null0" of
        Left (InvalidKeyword _) -> pure ()
        other ->
          expectationFailure $ "Expected InvalidKeyword, got: " <> show other

    it "keyword true followed by letter is invalid" $ do
      case decodeComplete "truex" of
        Left (InvalidKeyword _) -> pure ()
        other ->
          expectationFailure $ "Expected InvalidKeyword, got: " <> show other

    it "keyword false followed by brace is invalid" $ do
      case decodeComplete "false}" of
        Left (InvalidKeyword _) -> pure ()
        Left TrailingInput -> pure ()
        other ->
          expectationFailure
            $ "Expected InvalidKeyword or TrailingInput, got: "
            <> show other

    it "keyword null followed by space is valid" $ do
      decodeComplete "null " `shouldBe` Right [JSONNull]

    it "invalid escape in string at chunk boundary"
      $ case decodeChunks ["\"\\", "x\""] of
        Left (InvalidEscape 'x') -> pure ()
        other ->
          expectationFailure $ "Expected InvalidEscape, got: " <> show other

    it "incomplete high surrogate at EOF in streaming" $ do
      case decodeChunks ["\"\\uD834"] of
        Left InvalidSurrogatePair -> pure ()
        other ->
          expectationFailure
            $ "Expected InvalidSurrogatePair, got: "
            <> show other

    it "high surrogate followed by non-low in streaming" $ do
      case decodeChunks ["\"\\uD834", "\\u0041\""] of
        Left InvalidSurrogatePair -> pure ()
        other ->
          expectationFailure
            $ "Expected InvalidSurrogatePair, got: "
            <> show other

    it "number 1. at chunk boundary" $ case decodeChunks ["1.", ""] of
      Left (InvalidNumber _) -> pure ()
      Left UnexpectedEnd -> pure ()
      other ->
        expectationFailure
          $ "Expected InvalidNumber or UnexpectedEnd, got: "
          <> show other

    it "number 1e at chunk boundary" $ case decodeChunks ["1e", ""] of
      Left (InvalidNumber _) -> pure ()
      Left UnexpectedEnd -> pure ()
      other ->
        expectationFailure
          $ "Expected InvalidNumber or UnexpectedEnd, got: "
          <> show other

    it "number 1e+ at chunk boundary" $ case decodeChunks ["1e+", ""] of
      Left (InvalidNumber _) -> pure ()
      Left UnexpectedEnd -> pure ()
      other ->
        expectationFailure
          $ "Expected InvalidNumber or UnexpectedEnd, got: "
          <> show other

    it "valid number 1e10 across chunks" $ do
      runStreaming ["1e", "10"] `shouldBe` [JSONNumber 10000000000]

    it "valid number -1.5e-6 across chunks" $ do
      let expected = [JSONNumber (fromFloatDigits ((-1.5e-6) :: Double))]
      runStreaming ["-1.5e", "-6"] `shouldBe` expected

    it "nested object across chunks" $ do
      let expected =
            [ JSONBeginObject,
              JSONObjectKey "a",
              JSONBeginObject,
              JSONObjectKey "b",
              JSONNumber 1,
              JSONEndObject,
              JSONEndObject
            ]
      runStreaming ["{\"a\":{\"b\":", "1", "}}"] `shouldBe` expected

    it "trailing input after root value in streaming" $ do
      case decodeChunks ["42", " extra"] of
        Left TrailingInput -> pure ()
        other ->
          expectationFailure $ "Expected TrailingInput, got: " <> show other

--------------------------------------------------------------------------------
-- pullEvent (single-event pulls agree with streaming decode)
--------------------------------------------------------------------------------

-- | Pull every event through pullEvent.
pullAllChunks :: [Text] -> IO (Either Text [JSONEvent])
pullAllChunks chunks = do
  result <- runExceptT (collect initialDecoder stream)
  pure (first renderHQError result)
  where
    stream :: StreamIO Text ()
    stream = S.each chunks
    collect decoder text = do
      pulled <- pullEvent decoder text
      case pulled of
        EndOfInput -> pure []
        NextEvent event decoder' rest -> (event :) <$> collect decoder' rest

pullEventSpec :: Spec
pullEventSpec = describe "pullEvent" $ do
  it "agrees with streaming decode on valid inputs under every split" $ do
    forM_ validPullCorpus $ \input ->
      forM_ (splits input) $ \chunks -> do
        pulled <- pullAllChunks chunks
        pulled `shouldBe` decodeShown chunks

  it "agrees with streaming decode on malformed inputs under every split" $ do
    forM_ malformedPullCorpus $ \input ->
      forM_ (splits input) $ \chunks -> do
        pulled <- pullAllChunks chunks
        pulled `shouldBe` decodeShown chunks
