{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.JSONSpec (spec) where

import Data.Scientific (fromFloatDigits)
import qualified Data.Text as T
import HQ.JSON.Decoder
import HQ.JSON.Encoder (EncodeStyle (..), encode)
import HQ.JSON.Event
import Relude hiding (Compose, id)
import Streaming (Of (..))
import qualified Streaming.Prelude as S
import Test.Syd

spec :: Spec
spec = describe "HQ.JSON" $ do
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
  encodeSpec
  encodePrettySpec
  roundtripSpec

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Drain events from a decoder result by repeatedly calling step,
-- then finalize with finish.  Tries finish first to avoid infinite
-- loops when step returns NeedInput on an empty-input decoder.
drainCollect :: DecoderResult -> Either ParseError [JSONEvent]
drainCollect (Emit event next) = case step next of
  Left err -> Left err
  Right result -> fmap (event :) (drainCollect result)
drainCollect (Done _) = Right []
drainCollect (NeedInput next)
  -- There is still unconsumed input; step it rather than calling finish
  -- (finish discards decoderInput, which would lose surrogate pairs,
  -- keywords, and numbers that span across the last feed boundary).
  | not (T.null (decoderInput next)) = case step next of
      Left err -> Left err
      Right result -> drainCollect result
  | otherwise = case finish next of
      Right (Emit event next') -> case step next' of
        Left err' -> Left err'
        Right result' -> fmap (event :) (drainCollect result')
      Right (Done _) -> Right []
      Right (NeedInput _) -> Left UnexpectedEnd
      Left err -> Left err

-- | Decode input that is guaranteed to be complete (all at once).
decodeComplete :: Text -> Either ParseError [JSONEvent]
decodeComplete input = case feed input initialDecoder of
  Left err -> Left err
  Right result -> drainCollect result

-- | Decode text incrementally, feeding one character at a time.
decodeIncremental :: Text -> Either ParseError [JSONEvent]
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

-- | Stream text chunks through the decode function.
decodeStreaming :: [Text] -> Either ParseError [JSONEvent]
decodeStreaming chunks = runIdentity $ do
  result <- S.toList (decode (S.each chunks))
  case result of
    _ :> Left err -> pure (Left err)
    events :> Right _ -> pure (Right events)

-- | Helper to run a streaming decode and check the result.
runStreaming :: [Text] -> [JSONEvent]
runStreaming chunks = case decodeStreaming chunks of
  Left _err -> []
  Right events -> events

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
stringSpec = describe "strings" $ do
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
numberSpec = describe "numbers" $ do
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

  it "parses negative sign" $ do
    decodeComplete "-0" `shouldBe` Right [JSONNumber 0]

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
    case decodeStreaming ["4", "2"] of
      Left err -> expectationFailure $ "Parse error: " <> show err
      Right events -> events `shouldBe` [JSONNumber 42]

  it "parses object across 3 chunks" $ do
    let expected =
          [JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject]
    case decodeStreaming ["{\"a\":", "1", "}"] of
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

  it "reports error on invalid input" $ case decodeStreaming ["{invalid}"] of
    Left (UnexpectedChar 'i') -> pure ()
    Left _ -> pure ()
    Right events ->
      expectationFailure $ "Expected parse error, got events: " <> show events

  it "reports error on invalid character from non-empty stream" $ do
    case decodeStreaming ["{", "invalid}"] of
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
    case decodeStreaming ["42", " ", "extra"] of
      Left TrailingInput -> pure ()
      Left _ -> pure ()
      Right events ->
        expectationFailure $ "Expected TrailingInput, got: " <> show events

  it "reports error on empty chunk with trailing data"
    $ case decodeStreaming ["42", "extra"] of
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
adversarialSpec = describe "adversarial" $ do
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
      $ case decodeStreaming ["{\"foo\": @}}"] of
        Left (UnexpectedChar '@') -> pure ()
        Left _ -> pure ()
        Right events -> expectationFailure $ "Expected parse error, got: " <> show events

    it "propagates error after key in streaming" $ do
      case decodeStreaming ["{\"a\":", "@}"] of
        Left (UnexpectedChar '@') -> pure ()
        Left _ -> pure ()
        Right events ->
          expectationFailure $ "Expected parse error, got: " <> show events

    it "propagates error after colon in streaming" $ do
      case decodeStreaming ["{\"a\":@", "}"] of
        Left (UnexpectedChar '@') -> pure ()
        Left _ -> pure ()
        Right events ->
          expectationFailure $ "Expected parse error, got: " <> show events

    it "propagates error in deeply nested streaming" $ do
      case decodeStreaming ["[[[", "@]]]"] of
        Left (UnexpectedChar '@') -> pure ()
        Left _ -> pure ()
        Right events ->
          expectationFailure $ "Expected parse error, got: " <> show events

    it "propagates error after array element in streaming" $ do
      case decodeStreaming ["[1,", "@2]"] of
        Left (UnexpectedChar '@') -> pure ()
        Left _ -> pure ()
        Right events ->
          expectationFailure $ "Expected parse error, got: " <> show events

    it "propagates error on invalid keyword" $ do
      case decodeStreaming ["tru", "e"] of
        Right [JSONBool True] -> pure () -- "true" split across chunks is valid
        other ->
          expectationFailure $ "Expected [JSONBool True], got: " <> show other

    it "propagates string escape error at chunk boundary" $ do
      case decodeStreaming ["\"\\", "z\""] of
        Left (InvalidEscape 'z') -> pure ()
        Left _ -> pure ()
        Right events ->
          expectationFailure $ "Expected InvalidEscape, got: " <> show events

  -- ---- Empty and boundary ----
  describe "edge cases" $ do
    it "empty stream produces empty events" $ runStreaming [] `shouldBe` []

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
      $ case decodeStreaming ["\"\\", "x\""] of
        Left (InvalidEscape 'x') -> pure ()
        other ->
          expectationFailure $ "Expected InvalidEscape, got: " <> show other

    it "incomplete high surrogate at EOF in streaming" $ do
      case decodeStreaming ["\"\\uD834"] of
        Left InvalidSurrogatePair -> pure ()
        other ->
          expectationFailure
            $ "Expected InvalidSurrogatePair, got: "
            <> show other

    it "high surrogate followed by non-low in streaming" $ do
      case decodeStreaming ["\"\\uD834", "\\u0041\""] of
        Left InvalidSurrogatePair -> pure ()
        other ->
          expectationFailure
            $ "Expected InvalidSurrogatePair, got: "
            <> show other

    it "number 1. at chunk boundary" $ case decodeStreaming ["1.", ""] of
      Left (InvalidNumber _) -> pure ()
      Left UnexpectedEnd -> pure ()
      other ->
        expectationFailure
          $ "Expected InvalidNumber or UnexpectedEnd, got: "
          <> show other

    it "number 1e at chunk boundary" $ case decodeStreaming ["1e", ""] of
      Left (InvalidNumber _) -> pure ()
      Left UnexpectedEnd -> pure ()
      other ->
        expectationFailure
          $ "Expected InvalidNumber or UnexpectedEnd, got: "
          <> show other

    it "number 1e+ at chunk boundary" $ case decodeStreaming ["1e+", ""] of
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
      case decodeStreaming ["42", " extra"] of
        Left TrailingInput -> pure ()
        other ->
          expectationFailure $ "Expected TrailingInput, got: " <> show other

    it "multiple root values rejected" $ do
      case decodeStreaming ["42", " ", "extra"] of
        Left TrailingInput -> pure ()
        other ->
          expectationFailure $ "Expected TrailingInput, got: " <> show other

--------------------------------------------------------------------------------
-- encode / roundtrip
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

-- | Encode events to a single Text value using compact style.
encodeEvents :: [JSONEvent] -> Text
encodeEvents = encodeEventsWith Compact

-- | Encode events with the given style to a single Text value.
encodeEventsWith :: EncodeStyle -> [JSONEvent] -> Text
encodeEventsWith style events = runIdentity $ do
  result <- S.toList (encode style 32 (S.each events))
  case result of
    chunks :> _ -> pure $ decodeUtf8 $ mconcat chunks

--------------------------------------------------------------------------------
-- pretty encode
--------------------------------------------------------------------------------

encodePrettySpec :: Spec
encodePrettySpec = describe "pretty encode" $ do
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
        encodeEvents events `shouldBe` expected
        encodeEventsWith (Pretty 2) events `shouldBe` expected

  describe "empty containers stay compact" $ do
    it "empty object"
      $ encodeEventsWith (Pretty 2) [JSONBeginObject, JSONEndObject]
      `shouldBe` "{}"
    it "empty array"
      $ encodeEventsWith (Pretty 2) [JSONBeginArray, JSONEndArray]
      `shouldBe` "[]"
    it "empty containers nested in non-empty containers" $ do
      encodeEventsWith
        (Pretty 2)
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
        (Pretty 2)
        [JSONBeginArray, JSONBeginArray, JSONEndArray, JSONBeginArray, JSONEndArray, JSONEndArray]
        `shouldBe` "[\n  [],\n  []\n]"

  describe "single-element containers" $ do
    it "object"
      $ encodeEventsWith
        (Pretty 2)
        [JSONBeginObject, JSONObjectKey "name", JSONString "Alice", JSONEndObject]
      `shouldBe` "{\n  \"name\": \"Alice\"\n}"
    it "array"
      $ encodeEventsWith
        (Pretty 2)
        [JSONBeginArray, JSONNumber 42, JSONEndArray]
      `shouldBe` "[\n  42\n]"

  describe "multiple elements" $ do
    it "object with multiple fields"
      $ encodeEventsWith
        (Pretty 2)
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
        (Pretty 2)
        [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONNumber 3, JSONEndArray]
      `shouldBe` "[\n  1,\n  2,\n  3\n]"

  describe "nested structures" $ do
    it "object in object"
      $ encodeEventsWith
        (Pretty 2)
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
        (Pretty 2)
        [JSONBeginArray, JSONBeginArray, JSONNumber 1, JSONEndArray, JSONEndArray]
      `shouldBe` "[\n  [\n    1\n  ]\n]"
    it "array in object"
      $ encodeEventsWith
        (Pretty 2)
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
        (Pretty 2)
        [JSONBeginArray, JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject, JSONEndArray]
      `shouldBe` "[\n  {\n    \"a\": 1\n  }\n]"
    it "deeply nested"
      $ encodeEventsWith
        (Pretty 2)
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
        (Pretty 2)
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
        (Pretty 2)
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
      encodeEventsWith (Pretty 2) events
        `shouldBe` "[\n  \"quote\\\" backslash\\\\ newline\\n return\\r tab\\t bell\\b formfeed\\f control\\u0001 snowman☃ snow❄\"\n]"
    it "pretty output decodes back to the same events" $ do
      let events =
            [ JSONBeginArray,
              JSONString "tab\tyes\nline2\r\n\"quoted\"\\and 日本語☃\x07",
              JSONNumber (-1.5e-3),
              JSONEndArray
            ]
          encoded = encodeEventsWith (Pretty 2) events
      case decodeStreaming [encoded] of
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
        expected = encodeEventsWith (Pretty 2) events
    forM_ [1, 2, 3, 5, 8, 16, 24, 40] $ \cSize ->
      it ("chunk size " <> show cSize) $ do
        let concatenated = runIdentity $ do
              result <- S.toList (encode (Pretty 2) cSize (S.each events))
              case result of
                chunks :> _ -> pure $ decodeUtf8 $ mconcat chunks
        concatenated `shouldBe` expected
        case decodeStreaming [concatenated] of
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
  it "roundtrips mixed types in array"
    $ roundtrip "[1,\"hello\",true,null]"
  it "roundtrips deeply nested"
    $ roundtrip "{\"a\":{\"b\":{\"c\":\"deep\"}}}"
  it "roundtrips complex structure"
    $ roundtrip "{\"users\":[{\"name\":\"alice\",\"age\":30},{\"name\":\"bob\",\"age\":25}],\"count\":2}"
  it "roundtrips string with special escapes"
    $ roundtrip "\"tab\\there\\nnewline\\rret\""
  it "roundtrips string with unicode escape"
    $ roundtrip "\"\\u0041\""
  it "roundtrips number with exponent" $ roundtrip "1e10"
  it "roundtrips number with negative exponent" $ roundtrip "1e-2"
  it "roundtrips zero" $ roundtrip "0"

-- | Verify that encoding with 'Pretty 2' and decoding again yields the
-- same events as decoding the compact input, i.e. that pretty-printing
-- only changes whitespace.
prettyRoundtrip :: Text -> IO ()
prettyRoundtrip input = case decodeStreaming [input] of
  Left err -> expectationFailure $ "Decode failed: " <> show err
  Right events -> do
    let encoded = encodeEventsWith (Pretty 2) events
        redecoded = decodeStreaming [encoded]
    case redecoded of
      Left err -> expectationFailure $ "Re-decode failed: " <> show err
      Right events' -> events `shouldBe` events'

-- | Verify that encode . decode produces the same events (wrapped in Either).
roundtrip :: Text -> IO ()
roundtrip input = case decodeStreaming [input] of
  Left err -> expectationFailure $ "Decode failed: " <> show err
  Right events -> do
    let encoded = encodeEvents events
        redecoded = decodeStreaming [encoded]
    case redecoded of
      Left err -> expectationFailure $ "Re-decode failed: " <> show err
      Right events' -> events `shouldBe` events'
