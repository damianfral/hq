{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Shared helpers for the @hq@ test suite.
module Test.HQ
  ( decodeChunks,
    runStreaming,
    drainCollect,
    splits,
    chunkSplits,
    validPullCorpus,
    malformedPullCorpus,
    decodeShown,
  )
where

import Data.Text qualified as T
import HQ.JSON.Decoder (DecodeError (..), DecoderResult (..), DecoderState (..), decodeTexts, finish, step)
import HQ.JSON.Event (JSONEvent)
import Relude hiding (Compose, id)

-- | Stream text chunks through the decode function.
decodeChunks :: [Text] -> Either DecodeError [JSONEvent]
decodeChunks = decodeTexts

-- | Helper to run a streaming decode and check the result.
runStreaming :: [Text] -> [JSONEvent]
runStreaming chunks = case decodeChunks chunks of
  Left _err -> []
  Right events -> events

-- | Drain events from a decoder result by repeatedly calling step,
-- then finalize with finish.  Tries finish first to avoid infinite
-- loops when step returns NeedInput on an empty-input decoder.
drainCollect :: DecoderResult -> Either DecodeError [JSONEvent]
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

-- | Chunkings under which an input is decoded: whole, every two-way
-- split, and one-character chunks for short inputs.
splits :: Text -> [[Text]]
splits input
  | T.null input = [[input]]
  | otherwise =
      [input]
        : [[T.take n input, T.drop n input] | n <- [1 .. T.length input - 1]]
          ++ [[T.singleton c | c <- toString input] | T.length input <= 24]

-- | Streaming decode with errors shown, for differential testing.
decodeShown :: [Text] -> Either Text [JSONEvent]
decodeShown chunks = case decodeChunks chunks of
  Left err -> Left (show err)
  Right events -> Right events

-- | Chunkings: whole input, every two-way split, and one-character
-- chunks for short inputs.
chunkSplits :: Text -> [[Text]]
chunkSplits input
  | T.null input = [[input]]
  | otherwise =
      [input]
        : [[T.take n input, T.drop n input] | n <- [1 .. T.length input - 1]]
          ++ [[T.singleton c | c <- toString input] | T.length input <= 24]

--------------------------------------------------------------------------------
-- Shared corpus
--------------------------------------------------------------------------------

validPullCorpus :: [Text]
validPullCorpus =
  [ "42",
    "-42",
    "0",
    "3.14",
    "-3.14",
    "0.15",
    "1e10",
    "1E10",
    "1e-2",
    "1e+2",
    "1.5e2",
    "-1.5e-6",
    "-1e5",
    "null",
    "true",
    "false",
    "\"\"",
    "\"hello\"",
    "\"a\\\"b\\\\c\\/\\b\\f\\n\\r\\t\"",
    "\"\\u00e9\"",
    "\"\\ud83d\\ude00\"",
    "[\"\\ud83d\\ude00\"]",
    "{\"u\":\"\\ud83d\\ude00\"}",
    "[]",
    "[1,2,3]",
    "[1,\"a\",true,null]",
    "{}",
    "{\"a\":1,\"b\":[2,3]}",
    " {\"a\" : [1, 2] } ",
    "",
    "   "
  ]

malformedPullCorpus :: [Text]
malformedPullCorpus =
  [ "\"abc",
    "{\"a\":1",
    "[1,2",
    "12a",
    "\"\\x\"",
    "\"\\u00z1\"",
    "\"\\ud83d\"",
    "\"\\ud83dX\"",
    "01",
    "1e",
    "1e+",
    "{\"a\" 1}",
    "[1,,2]",
    "[1 2]",
    "{,}",
    "nul",
    "-",
    "--1"
  ]
