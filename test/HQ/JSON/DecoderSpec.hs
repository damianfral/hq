{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.JSON.DecoderSpec (spec) where

import Data.Scientific (fromFloatDigits)
import HQ.Early (runEarly)
import HQ.JSON.Decoder
import HQ.JSON.Event
import Relude hiding (Compose, id)
import Streaming.Prelude qualified as S
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
      Right (Emit event nextDec) -> fmap (event :) (go nextDec cs)
      Right (NeedInput nextDec) -> go nextDec cs
      Right (Done nextDec) -> go nextDec cs

-- | Assert a decode fails with an accepted error.
shouldFailWith :: Either DecodeError [JSONEvent] -> (DecodeError -> Bool) -> IO ()
shouldFailWith (Left err) p
  | p err = pure ()
  | otherwise = expectationFailure $ "Unexpected decode error: " <> show err
shouldFailWith (Right events) _ =
  expectationFailure $ "Expected decode error, got events: " <> show events

--------------------------------------------------------------------------------
-- Scalar values
--------------------------------------------------------------------------------

scalarSpec :: Spec
scalarSpec = describe "scalars" $ forM_ scalarCases $ \(label, input, expected) ->
  it label $ decodeComplete input `shouldBe` Right expected

scalarCases :: [(String, Text, [JSONEvent])]
scalarCases =
  [ ("parses null", "null", [JSONNull]),
    ("parses true", "true", [JSONBool True]),
    ("parses false", "false", [JSONBool False]),
    ("parses zero", "0", [JSONNumber 0]),
    ("parses positive integer", "42", [JSONNumber 42]),
    ("parses negative integer", "-42", [JSONNumber (-42)]),
    ("parses decimal", "3.14", [JSONNumber (fromFloatDigits (3.14 :: Double))]),
    ( "parses negative decimal",
      "-3.14",
      [JSONNumber (fromFloatDigits (-3.14 :: Double))]
    ),
    ("parses scientific notation", "1e10", [JSONNumber 10000000000]),
    ("parses scientific notation with exponent", "1.5e2", [JSONNumber 150]),
    ( "parses negative mantissa with unsigned exponent",
      "-1e5",
      [JSONNumber (-100000)]
    ),
    ("parses negative decimal with exponent", "-1.5e2", [JSONNumber (-150)]),
    ("parses empty string", "\"\"", [JSONString ""]),
    ("parses simple string", "\"hello\"", [JSONString "hello"]),
    ("parses string with spaces", "\"hello world\"", [JSONString "hello world"])
  ]

--------------------------------------------------------------------------------
-- String escaping
--------------------------------------------------------------------------------

stringSpec :: Spec
stringSpec = describe "string escape sequences" $ do
  forM_ stringCases $ \(label, input, expected) ->
    it label $ decodeComplete input `shouldBe` Right expected

  it "rejects invalid escape character"
    $ decodeComplete "\"hello\\xworld\""
    `shouldFailWith` (== InvalidEscape 'x')

stringCases :: [(String, Text, [JSONEvent])]
stringCases =
  [ ("parses escaped quote", "\"hello\\\"world\"", [JSONString "hello\"world"]),
    ( "parses escaped backslash",
      "\"hello\\\\world\"",
      [JSONString "hello\\world"]
    ),
    ("parses escaped newline", "\"hello\\nworld\"", [JSONString "hello\nworld"]),
    ("parses escaped tab", "\"hello\\tworld\"", [JSONString "hello\tworld"]),
    ( "parses escaped carriage return",
      "\"hello\\rworld\"",
      [JSONString "hello\rworld"]
    ),
    ( "parses escaped backspace",
      "\"hello\\bworld\"",
      [JSONString "hello\bworld"]
    ),
    ( "parses escaped form feed",
      "\"hello\\fworld\"",
      [JSONString "hello\fworld"]
    ),
    ( "parses escaped forward slash",
      "\"hello\\/world\"",
      [JSONString "hello/world"]
    ),
    ("parses unicode escape", "\"\\u0041\"", [JSONString "A"]),
    ("parses multi-byte unicode escape", "\"\\u00E9\"", [JSONString "\233"]),
    -- U+1D11E (Musical Symbol G Clef) = D834 DD1E
    ("parses surrogate pair", "\"\\uD834\\uDD1E\"", [JSONString "\119070"]),
    ( "does not consume a hex digit following a unicode escape",
      "\"\\u0026B\"",
      [JSONString "&B"]
    ),
    ( "does not consume a trailing hex letter",
      "\"\\u0041B\"",
      [JSONString "AB"]
    ),
    ( "parses string with multiple escapes",
      "\"line1\\nline2\\ttab\"",
      [JSONString "line1\nline2\ttab"]
    )
  ]

--------------------------------------------------------------------------------
-- Numbers
--------------------------------------------------------------------------------

numberSpec :: Spec
numberSpec = describe "number formats" $ do
  forM_ numberCases $ \(label, input, expected) ->
    it label $ decodeComplete input `shouldBe` Right expected

numberCases :: [(String, Text, [JSONEvent])]
numberCases =
  [ ("parses single digit", "5", [JSONNumber 5]),
    ("parses multiple digits", "123", [JSONNumber 123]),
    ("parses leading zero", "0", [JSONNumber 0]),
    ("parses negative zero", "-0", [JSONNumber 0]),
    ("parses with fractional part", "1.0", [JSONNumber 1]),
    ("parses with exponent E", "1E10", [JSONNumber 10000000000]),
    ("parses with exponent e", "1e10", [JSONNumber 10000000000]),
    ( "parses with negative exponent",
      "1e-2",
      [JSONNumber (fromFloatDigits (0.01 :: Double))]
    ),
    ("parses with positive exponent sign", "1e+2", [JSONNumber 100]),
    ( "parses large number",
      "99999999999999999999",
      [JSONNumber 99999999999999999999]
    ),
    ( "parses -1.5e-6",
      "-1.5e-6",
      [JSONNumber (fromFloatDigits ((-1.5e-6) :: Double))]
    ),
    ("parses 1.25", "1.25", [JSONNumber (fromFloatDigits (1.25 :: Double))]),
    ("parses 1E+10", "1E+10", [JSONNumber 10000000000])
  ]

--------------------------------------------------------------------------------
-- Objects
--------------------------------------------------------------------------------

objectSpec :: Spec
objectSpec = describe "objects" $ forM_ objectCases $ \(label, input, expected) ->
  it label $ decodeComplete input `shouldBe` Right expected

objectCases :: [(String, Text, [JSONEvent])]
objectCases =
  [ ("parses empty object", "{}", [JSONBeginObject, JSONEndObject]),
    ( "parses object with one field",
      "{\"name\":\"alice\"}",
      [JSONBeginObject, JSONObjectKey "name", JSONString "alice", JSONEndObject]
    ),
    ( "parses object with multiple fields",
      "{\"a\":1,\"b\":2}",
      [ JSONBeginObject,
        JSONObjectKey "a",
        JSONNumber 1,
        JSONObjectKey "b",
        JSONNumber 2,
        JSONEndObject
      ]
    ),
    ( "parses object with string value",
      "{\"key\":\"value\"}",
      [JSONBeginObject, JSONObjectKey "key", JSONString "value", JSONEndObject]
    ),
    ( "parses object with null value",
      "{\"x\":null}",
      [JSONBeginObject, JSONObjectKey "x", JSONNull, JSONEndObject]
    ),
    ( "parses object with boolean value",
      "{\"flag\":true}",
      [JSONBeginObject, JSONObjectKey "flag", JSONBool True, JSONEndObject]
    ),
    ( "parses object with number value",
      "{\"count\":42}",
      [JSONBeginObject, JSONObjectKey "count", JSONNumber 42, JSONEndObject]
    ),
    ( "parses object with whitespace",
      "{ \"name\" : \"alice\" }",
      [JSONBeginObject, JSONObjectKey "name", JSONString "alice", JSONEndObject]
    )
  ]

--------------------------------------------------------------------------------
-- Arrays
--------------------------------------------------------------------------------

arraySpec :: Spec
arraySpec = describe "arrays" $ forM_ arrayCases $ \(label, input, expected) ->
  it label $ decodeComplete input `shouldBe` Right expected

arrayCases :: [(String, Text, [JSONEvent])]
arrayCases =
  [ ("parses empty array", "[]", [JSONBeginArray, JSONEndArray]),
    ( "parses array with one element",
      "[1]",
      [JSONBeginArray, JSONNumber 1, JSONEndArray]
    ),
    ( "parses array with multiple elements",
      "[1,2,3]",
      [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONNumber 3, JSONEndArray]
    ),
    ( "parses array with mixed types",
      "[1,\"hello\",true,null]",
      [ JSONBeginArray,
        JSONNumber 1,
        JSONString "hello",
        JSONBool True,
        JSONNull,
        JSONEndArray
      ]
    ),
    ( "parses array with whitespace",
      "[ 1 , 2 , 3 ]",
      [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONNumber 3, JSONEndArray]
    ),
    ( "parses array of strings",
      "[\"a\",\"b\",\"c\"]",
      [JSONBeginArray, JSONString "a", JSONString "b", JSONString "c", JSONEndArray]
    ),
    ( "parses array of booleans",
      "[true,false,true]",
      [JSONBeginArray, JSONBool True, JSONBool False, JSONBool True, JSONEndArray]
    )
  ]

--------------------------------------------------------------------------------
-- Nested structures
--------------------------------------------------------------------------------

nestedSpec :: Spec
nestedSpec = describe "nested structures" $ do
  forM_ nestedCases $ \(label, input, expected) ->
    it label $ decodeComplete input `shouldBe` Right expected

  it "parses complex real-world-like structure" $ do
    let input = "{\"users\":[{\"name\":\"alice\",\"age\":30},{\"name\":\"bob\",\"age\":25}],\"count\":2}"
    case decodeComplete input of
      Left err -> expectationFailure $ "Parse error: " <> show err
      Right events -> do
        viaNonEmpty head events `shouldBe` Just JSONBeginObject
        length events `shouldSatisfy` (> 10)

nestedCases :: [(String, Text, [JSONEvent])]
nestedCases =
  [ ( "parses nested objects",
      "{\"a\":{\"b\":42}}",
      [ JSONBeginObject,
        JSONObjectKey "a",
        JSONBeginObject,
        JSONObjectKey "b",
        JSONNumber 42,
        JSONEndObject,
        JSONEndObject
      ]
    ),
    ( "parses nested arrays",
      "[[1,2],[3,4]]",
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
    ),
    ( "parses object containing array",
      "{\"items\":[1,2,3]}",
      [ JSONBeginObject,
        JSONObjectKey "items",
        JSONBeginArray,
        JSONNumber 1,
        JSONNumber 2,
        JSONNumber 3,
        JSONEndArray,
        JSONEndObject
      ]
    ),
    ( "parses array containing object",
      "[{\"a\":1}]",
      [ JSONBeginArray,
        JSONBeginObject,
        JSONObjectKey "a",
        JSONNumber 1,
        JSONEndObject,
        JSONEndArray
      ]
    ),
    ( "parses deeply nested structure",
      "{\"a\":{\"b\":{\"c\":\"deep\"}}}",
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
    )
  ]

--------------------------------------------------------------------------------
-- Incremental parsing
--------------------------------------------------------------------------------

incrementalSpec :: Spec
incrementalSpec = describe "incremental parsing" $ do
  forM_ incrementalCases $ \(label, input, expected) ->
    it label $ decodeIncremental input `shouldBe` Right expected

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

incrementalCases :: [(String, Text, [JSONEvent])]
incrementalCases =
  [ ("parses scalar incrementally", "42", [JSONNumber 42]),
    ("parses string incrementally", "\"hi\"", [JSONString "hi"]),
    ( "parses object incrementally",
      "{\"a\":1}",
      [JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject]
    ),
    ( "parses array incrementally",
      "[1,2,3]",
      [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONNumber 3, JSONEndArray]
    ),
    ( "parses nested incrementally",
      "[[1],[2]]",
      [ JSONBeginArray,
        JSONBeginArray,
        JSONNumber 1,
        JSONEndArray,
        JSONBeginArray,
        JSONNumber 2,
        JSONEndArray,
        JSONEndArray
      ]
    )
  ]

--------------------------------------------------------------------------------
-- Error handling
--------------------------------------------------------------------------------

errorSpec :: Spec
errorSpec = describe "error handling" $ forM_ errorCases $ \(label, input, p) ->
  it label $ decodeComplete input `shouldFailWith` p

errorCases :: [(String, Text, DecodeError -> Bool)]
errorCases =
  [ ("rejects empty input", "", (== UnexpectedEnd)),
    ("rejects incomplete object", "{\"a\":", (== UnexpectedEnd)),
    ("rejects trailing input", "42 extra", (== TrailingInput)),
    ("rejects trailing input after object", "{} {}", (== TrailingInput)),
    ( "rejects invalid keyword",
      "nul",
      \case
        InvalidKeyword _ -> True
        UnexpectedEnd -> True
        _ -> False
    ),
    ("rejects invalid character", "x", (== UnexpectedChar 'x')),
    ("rejects unmatched opening bracket", "[", (== UnexpectedEnd)),
    ( "rejects unmatched closing bracket",
      "]",
      \e ->
        e == UnexpectedChar ']' || e == UnexpectedEnd
    ),
    ("rejects missing colon in object", "{\"a\" 1}", (== ExpectedColon)),
    ("rejects missing comma in array", "[1 2]", (== ExpectedCommaOrEnd)),
    ("rejects invalid escape in string", "\"\\z\"", (== InvalidEscape 'z')),
    ("rejects invalid unicode escape", "\"\\uGGGG\"", (== InvalidUnicodeEscape)),
    ( "rejects invalid number --",
      "--",
      \case
        InvalidNumber _ -> True
        UnexpectedChar '-' -> True
        _ -> False
    )
  ]

--------------------------------------------------------------------------------
-- Streaming adapter
--------------------------------------------------------------------------------

streamingSpec :: Spec
streamingSpec = describe "streaming decode" $ do
  forM_ streamingCases $ \(label, chunks, expected) ->
    it label $ runStreaming chunks `shouldBe` expected

  forM_ streamingErrorChunks $ \chunks ->
    it ("rejects " <> show chunks) $ decodeChunks chunks `shouldSatisfy` isLeft

  it "reports trailing input after root value"
    $ decodeChunks ["42", " extra"]
    `shouldFailWith` (== TrailingInput)

streamingCases :: [(String, [Text], [JSONEvent])]
streamingCases =
  [ ("decodes a single chunk", ["42"], [JSONNumber 42]),
    ( "decodes multiple chunks",
      ["{", "\"a\"", ":", "1", "}"],
      [JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject]
    ),
    ("decodes empty stream", [], []),
    ("decodes scalar values", ["null"], [JSONNull]),
    ("decodes boolean", ["true"], [JSONBool True]),
    ("decodes string across chunks", ["\"hel", "lo\""], [JSONString "hello"]),
    ( "decodes array across chunks",
      ["[1,", "2,", "3]"],
      [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONNumber 3, JSONEndArray]
    ),
    ( "decodes object across chunks",
      ["{\"a\":", "1}"],
      [JSONBeginObject, JSONObjectKey "a", JSONNumber 1, JSONEndObject]
    ),
    ( "decodes nested structure across chunks",
      ["[[1],", "[2]]"],
      [ JSONBeginArray,
        JSONBeginArray,
        JSONNumber 1,
        JSONEndArray,
        JSONBeginArray,
        JSONNumber 2,
        JSONEndArray,
        JSONEndArray
      ]
    ),
    ( "decodes single-character chunks",
      ["[", "1", ",", "2", "]"],
      [JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONEndArray]
    ),
    ( "decodes string literal across 5 chunks",
      ["\"", "he", "llo", " wor", "ld\""],
      [JSONString "hello world"]
    ),
    ( "decodes complex nested across many chunks",
      ["{\"arr\":[", "1,2],", "\"key\":\"val\"}"],
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
    ),
    ( "parses surrogate pair across chunks",
      ["\"\\uD834", "\\uDD1E\""],
      [JSONString "\119070"]
    ),
    ( "parses unicode escape split across chunks followed by hex digit",
      ["\"\\u004", "1B\""],
      [JSONString "AB"]
    ),
    ( "parses object with unicode value",
      ["{\"k\":\"\\u0041\"}"],
      [JSONBeginObject, JSONObjectKey "k", JSONString "A", JSONEndObject]
    ),
    ( "parses nested arrays across many chunks",
      ["[[", "[", "1", "]", "]]"],
      [ JSONBeginArray,
        JSONBeginArray,
        JSONBeginArray,
        JSONNumber 1,
        JSONEndArray,
        JSONEndArray,
        JSONEndArray
      ]
    ),
    ( "parses escape sequences at chunk boundary",
      ["\"\\", "n\""],
      [JSONString "\n"]
    ),
    ( "parses deeply nested object across chunks",
      ["{\"a\":{\"b\":", "{\"", "c", "\":", "42}}}"],
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
    ),
    ("parses number 0 across chunks", ["0"], [JSONNumber 0]),
    ("parses number -42 across chunks", ["-4", "2"], [JSONNumber (-42)]),
    ( "parses fractional number across chunks",
      ["3.", "14"],
      [JSONNumber (fromFloatDigits (3.14 :: Double))]
    ),
    ( "parses exponent number across chunks",
      ["1e", "10"],
      [JSONNumber 10000000000]
    ),
    ("single whitespace stream is empty", [" "], []),
    ("multiple empty chunks then data", ["", "", "42"], [JSONNumber 42]),
    ("data then empty chunks", ["42", "", ""], [JSONNumber 42]),
    ("valid number 1e10 across chunks", ["1e", "10"], [JSONNumber 10000000000]),
    ( "valid number -1.5e-6 across chunks",
      ["-1.5e", "-6"],
      [JSONNumber (fromFloatDigits ((-1.5e-6) :: Double))]
    ),
    ( "nested object across chunks",
      ["{\"a\":{\"b\":", "1", "}}"],
      [ JSONBeginObject,
        JSONObjectKey "a",
        JSONBeginObject,
        JSONObjectKey "b",
        JSONNumber 1,
        JSONEndObject,
        JSONEndObject
      ]
    )
  ]

streamingErrorChunks :: [[Text]]
streamingErrorChunks =
  [["{invalid}"], ["{", "invalid}"], ["42", " ", "extra"], ["42", "extra"]]

--------------------------------------------------------------------------------
-- Adversarial tests: invalid numbers, surrogate pairs, error propagation
--------------------------------------------------------------------------------

adversarialSpec :: Spec
adversarialSpec = describe "malformed input" $ do
  -- ---- Invalid numbers ----
  describe "invalid numbers"
    $ forM_ invalidNumberCases
    $ \(label, input, p) ->
      it label $ decodeComplete input `shouldFailWith` p

  -- ---- Surrogate pair errors at EOF ----
  describe "surrogate pair errors"
    $ forM_ surrogateCases
    $ \(label, input, p) ->
      it label $ decodeComplete input `shouldFailWith` p

  -- ---- Error propagation in streaming (the key continueWith bug) ----
  describe "error propagation" $ do
    forM_ propagationChunks $ \chunks ->
      it ("propagates an error from " <> show chunks)
        $ decodeChunks chunks
        `shouldSatisfy` isLeft

    it "parses keyword true split across chunks"
      $ decodeChunks ["tru", "e"]
      `shouldBe` Right [JSONBool True]

    it "propagates string escape error at chunk boundary"
      $ decodeChunks ["\"\\", "z\""]
      `shouldFailWith` (== InvalidEscape 'z')

  -- ---- Empty and boundary ----
  describe "edge cases" $ do
    forM_ edgeCases $ \(label, input, expected) ->
      it label $ decodeComplete input `shouldBe` Right expected

    it "rejects unterminated input" $ forM_ unterminatedCases $ \input ->
      decodeComplete input `shouldFailWith` (== UnexpectedEnd)

    forM_ keywordCases $ \(label, input, p) ->
      it label $ decodeComplete input `shouldFailWith` p

    it "keyword null without delimiter at EOF" $ do
      decodeComplete "null" `shouldBe` Right [JSONNull]

    it "keyword null followed by space is valid" $ do
      decodeComplete "null " `shouldBe` Right [JSONNull]

    it "invalid escape in string at chunk boundary"
      $ decodeChunks ["\"\\", "x\""]
      `shouldFailWith` (== InvalidEscape 'x')

    it "incomplete high surrogate at EOF in streaming"
      $ decodeChunks ["\"\\uD834"]
      `shouldFailWith` (== InvalidSurrogatePair)

    it "high surrogate followed by non-low in streaming"
      $ decodeChunks ["\"\\uD834", "\\u0041\""]
      `shouldFailWith` (== InvalidSurrogatePair)

    it "rejects truncated numbers at chunk boundaries"
      $ forM_ [["1.", ""], ["1e", ""], ["1e+", ""]]
      $ \chunks ->
        decodeChunks chunks
          `shouldFailWith` (\e -> isInvalidNumber e || e == UnexpectedEnd)

edgeCases :: [(String, Text, [JSONEvent])]
edgeCases =
  [ ( "deeply nested empty arrays",
      "[[[[]]]]",
      [ JSONBeginArray,
        JSONBeginArray,
        JSONBeginArray,
        JSONBeginArray,
        JSONEndArray,
        JSONEndArray,
        JSONEndArray,
        JSONEndArray
      ]
    ),
    ( "all scalar types in array",
      "[null,true,false,0,1.5,\"x\"]",
      [ JSONBeginArray,
        JSONNull,
        JSONBool True,
        JSONBool False,
        JSONNumber 0,
        JSONNumber (fromFloatDigits (1.5 :: Double)),
        JSONString "x",
        JSONEndArray
      ]
    ),
    ( "empty object value",
      "{\"a\":{}}",
      [ JSONBeginObject,
        JSONObjectKey "a",
        JSONBeginObject,
        JSONEndObject,
        JSONEndObject
      ]
    ),
    ( "empty array value",
      "{\"a\":[]}",
      [ JSONBeginObject,
        JSONObjectKey "a",
        JSONBeginArray,
        JSONEndArray,
        JSONEndObject
      ]
    )
  ]

unterminatedCases :: [Text]
unterminatedCases =
  [ "\"hello",
    "\"hello\\",
    "\"\\u00",
    "{\"a\":1",
    "[1,2"
  ]

keywordCases :: [(String, Text, DecodeError -> Bool)]
keywordCases =
  [ ("keyword null followed by digit is invalid", "null0", isInvalidKeyword),
    ("keyword true followed by letter is invalid", "truex", isInvalidKeyword),
    ( "keyword false followed by brace is invalid",
      "false}",
      \e -> isInvalidKeyword e || e == TrailingInput
    )
  ]

isInvalidKeyword :: DecodeError -> Bool
isInvalidKeyword (InvalidKeyword _) = True
isInvalidKeyword _ = False

invalidNumberCases :: [(String, Text, DecodeError -> Bool)]
invalidNumberCases =
  [ ( "rejects +1 (leading plus)",
      "+1",
      \e -> e == UnexpectedChar '+' || isInvalidNumber e
    ),
    ("rejects 01 (leading zero)", "01", isInvalidNumber),
    ( "rejects 1. (trailing dot)",
      "1.",
      \e -> isInvalidNumber e || e == UnexpectedEnd
    ),
    ( "rejects 1e (trailing exponent)",
      "1e",
      \e -> isInvalidNumber e || e == UnexpectedEnd
    ),
    ( "rejects 1e+ (trailing exponent sign)",
      "1e+",
      \e -> isInvalidNumber e || e == UnexpectedEnd
    ),
    ( "rejects - (just a sign)",
      "-",
      \e -> isInvalidNumber e || e == UnexpectedEnd
    ),
    ( "rejects 1--2 (double negative)",
      "1--2",
      \e -> isInvalidNumber e || e == UnexpectedChar '-'
    ),
    ( "rejects 1.2.3 (double dot)",
      "1.2.3",
      \e -> isInvalidNumber e || e == UnexpectedChar '.'
    ),
    ( "rejects -. (sign + dot only)",
      "-.",
      \e -> isInvalidNumber e || e == UnexpectedChar '.'
    ),
    ( "rejects 1e--2 (double exponent sign)",
      "1e--2",
      \e -> isInvalidNumber e || e == UnexpectedChar '-'
    )
  ]

surrogateCases :: [(String, Text, DecodeError -> Bool)]
surrogateCases =
  [ ( "rejects incomplete high surrogate at EOF",
      "\"\\uD834",
      \e -> e == InvalidSurrogatePair || e == UnexpectedEnd
    ),
    ( "rejects high surrogate followed by non-low-surrogate",
      "\"\\uD834\\u0041\"",
      (== InvalidSurrogatePair)
    ),
    ("rejects lone low surrogate", "\"\\uDC00\"", (== InvalidSurrogatePair)),
    ( "rejects incomplete low surrogate escape at EOF",
      "\"\\uD834\\u",
      \e -> e == InvalidSurrogatePair || e == InvalidUnicodeEscape || e == UnexpectedEnd
    ),
    ( "rejects high surrogate followed by invalid unicode",
      "\"\\uD834\\uGGGG\"",
      \e -> e == InvalidSurrogatePair || e == InvalidUnicodeEscape
    )
  ]

propagationChunks :: [[Text]]
propagationChunks =
  [ ["{\"foo\": @}}"],
    ["{\"a\":", "@}"],
    ["{\"a\":@", "}"],
    ["[[[", "@]]]"],
    ["[1,", "@2]"]
  ]

isInvalidNumber :: DecodeError -> Bool
isInvalidNumber (InvalidNumber _) = True
isInvalidNumber _ = False

--------------------------------------------------------------------------------
-- pullEvent (single-event pulls agree with streaming decode)
--------------------------------------------------------------------------------

-- | Pull every event through pullEvent.
pullAllChunks :: [Text] -> IO (Either Text [JSONEvent])
pullAllChunks chunks = do
  result <- runEarly (\early -> collect early initialDecoder stream)
  pure (first renderHQError result)
  where
    stream :: StreamIO Text ()
    stream = S.each chunks
    collect early decoder text = do
      pulled <- pullEvent early decoder text
      case pulled of
        EndOfInput -> pure []
        NextEvent (NextEmitted event decoder' rest) ->
          (event :) <$> collect early decoder' rest

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
