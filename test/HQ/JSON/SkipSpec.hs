{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.JSON.SkipSpec (spec) where

import Control.Monad.Error.Class (throwError)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.Text as T
import HQ.JSON.Decoder
import HQ.JSON.Decoder.Number (startNumberState)
import HQ.JSON.Depth (NestDepth (NestDepth), initialDepth)
import HQ.JSON.Event (JSONEvent (..))
import HQ.JSON.Skip
import Relude hiding (Compose, id)
import qualified Streaming.Prelude as S
import Test.HQ (decodeShown, drainCollect, runStreaming, splits)
import Test.Syd

spec :: Spec
spec = describe "HQ.JSON.Skip" $ do
  skipTextSpec
  skipContainerTextSpec
  collectSpec
  collectNumberSpec

-- | Map a decoded event list to the skip-result shape: errors pass
-- through, successes mean exact consumption.
firstShown :: Either Text [JSONEvent] -> Either Text (Text, [Text])
firstShown (Left err) = Left err
firstShown (Right _) = Right (mempty, [])

--------------------------------------------------------------------------------
-- skipValueText / skipMemberValueText
--------------------------------------------------------------------------------

-- | Skip an object member value over chunks starting after the key.
runSkipMember :: [Text] -> IO (Either Text (Text, [Text]))
runSkipMember [] = pure (Left "empty chunk list")
runSkipMember (c : cs) = do
  result <- runExceptT $ do
    (remText, rest) <- skipMemberValueText (NestDepth 0) [DecodeObject] c (S.each cs)
    remaining <- S.toList_ rest
    pure (remText, remaining)
  pure (first renderHQError result)

-- | Drain a hand-positioned mid-object decoder: after skipping member
-- @a@, the rest must decode to the remaining members.
drainAfterSkippedMember :: Text -> Either DecodeError [JSONEvent]
drainAfterSkippedMember remainder =
  let decoder =
        DecoderState
          { decoderInput = remainder,
            decoderStack = [DecodeObject],
            decoderNestDepth = initialDepth,
            decoderPhase = DecoderPhaseObjectComma
          }
   in case step decoder of
        Left err -> Left err
        Right result -> drainCollect result

skipTextSpec :: Spec
skipTextSpec = describe "skipMember" $ do
  it "skips member values and continues decoding after every split" $ do
    forM_ memberValues $ \value ->
      let full = "{\"a\":" <> value <> ",\"b\":2}"
          keyLength = T.length "{\"a\""
          consumedLength = keyLength + 1 + T.length value
          fullRemainder = T.drop consumedLength full
       in forM_ [keyLength .. T.length full] $ \n -> do
            let memberChunks = [T.drop keyLength (T.take n full), T.drop n full]
            skipped <- runSkipMember memberChunks
            case skipped of
              Left err -> expectationFailure $ "skip failed: " <> toString err
              Right (remHead, remChunks) -> do
                let remainder = remHead <> T.concat remChunks
                remainder `shouldBe` fullRemainder
                drainAfterSkippedMember remainder
                  `shouldBe` Right [JSONObjectKey "b", JSONNumber 2, JSONEndObject]

  it "reports member value errors like streaming decode" $ do
    forM_ memberMalformed $ \full ->
      let prefixLength = T.length "{\"a\""
       in forM_ [prefixLength .. T.length full] $ \n -> do
            let chunks = [T.take n full, T.drop n full]
                memberChunks = [T.drop prefixLength (T.take n full), T.drop n full]
            skipped <- runSkipMember memberChunks
            skipped `shouldBe` firstShown (decodeShown chunks)

memberValues :: [Text]
memberValues =
  [ "1",
    "\"x\"",
    "true",
    "null",
    "[1,2]",
    "{\"x\":1}",
    "1.5e2",
    "-1e5",
    "\"a\\\"b\"",
    "\"\\ud83d\\ude00\""
  ]

memberMalformed :: [Text]
memberMalformed =
  [ "{\"a\":01}",
    "{\"a\":\"\\x\"}",
    "{\"a\":1e}",
    "{\"a\" 1}",
    "{\"a\":nul}"
  ]

-- | Pull every remaining event through pullEvent.
pullRemaining :: DecoderState -> StreamIO Text () -> ExceptT HQError IO [JSONEvent]
pullRemaining decoder text = do
  pulled <- pullEvent decoder text
  case pulled of
    EndOfInput -> pure []
    NextEvent event decoder' rest -> (event :) <$> pullRemaining decoder' rest

-- | Skip a string body over chunks starting after the opening quote.
runSkipString :: [Text] -> IO (Either Text Text)
runSkipString [] = pure (Left "empty chunk list")
runSkipString (c : cs) = do
  result <- runExceptT $ do
    (remHead, rest) <- skipStringText c (S.each cs)
    remChunks <- S.toList_ rest
    pure (remHead <> T.concat remChunks)
  pure (first renderHQError result)

-- | Capture a string body over the same chunks: remainder, captured
-- bytes and estimated size.
runCollectString :: [Text] -> IO (Either Text (Text, Text, Int))
runCollectString [] = pure (Left "empty chunk list")
runCollectString (c : cs) = do
  result <- runExceptT $ do
    (remHead, rawB, rawS, rest) <- skipStringCollect c (S.each cs)
    remChunks <- S.toList_ rest
    pure (remHead <> T.concat remChunks, decodeUtf8 (toStrict (toLazyByteString rawB)), rawS)
  pure (first renderHQError result)

collectSpec :: Spec
collectSpec = describe "skipStringCollect" $ do
  it "captures exactly the bytes it skips across every split" $ do
    forM_ collectCorpus $ \full ->
      forM_ (splits (T.drop 1 full)) $ \chunks -> do
        skipped <- runSkipString chunks
        collected <- runCollectString chunks
        case (skipped, collected) of
          (Left err1, Left err2) -> err1 `shouldBe` err2
          (Right rem1, Right (rem2, bytes, size)) -> do
            rem1 `shouldBe` rem2
            let total = T.concat chunks
                consumed = T.take (T.length total - T.length rem2) total
                -- The capture excludes quotes: drop the closing one.
                body = T.dropEnd 1 consumed
            bytes `shouldBe` body
            size `shouldBe` T.length body
          (a, b) -> expectationFailure $ "mismatch: " <> show (a, b)
  where
    collectCorpus :: [Text]
    collectCorpus =
      [ "\"\"",
        "\"hello\"",
        "\"x\"",
        "\"lorem ipsum dolor sit amet\"",
        "\"a\\\"b\\\\c\\/d\\be\\ff\\ng\\rh\\ti\"",
        "\"\\\"\"",
        "\"\\\\\"",
        "\"tab\\there\"",
        "\"\\u00e9\"",
        "\"\\ud83d\\ude00\"",
        "\"\\x\"",
        "\"abc",
        "\"\\u00z1\"",
        "\"\\ud83d\"",
        "\"\\ud83dX\""
      ]

skipContainerTextSpec :: Spec
skipContainerTextSpec = describe "skipContainerText" $ do
  it "skips pulled containers and continues decoding after every split" $ do
    forM_ containerValues $ \value ->
      let full = "[" <> value <> ",99]"
       in forM_ (splits full) $ \chunks -> do
            result <- runExceptT $ do
              let textStream :: StreamIO Text ()
                  textStream = S.each chunks
              openArray <- pullEvent initialDecoder textStream
              case openArray of
                NextEvent JSONBeginArray decoder1 text1 -> do
                  openMember <- pullEvent decoder1 text1
                  case openMember of
                    NextEvent open decoder2 text2 -> do
                      (decoder3, text3) <- skipContainerText open decoder2 text2
                      pullRemaining decoder3 text3
                    _ -> throwError (HQDecodeError (UnexpectedToken "expected a member opener"))
                _ -> throwError (HQDecodeError (UnexpectedToken "expected an array opener"))
            let reference = drop (1 + length (runStreaming [value])) (runStreaming [full])
            result `shouldBe` Right reference
  where
    containerValues :: [Text]
    containerValues =
      [ "[1,2]",
        "{\"a\":1}",
        "[1,{\"a\":[2]}]",
        "[[[]]]",
        "{\"a\":[1,{\"b\":2}]}"
      ]

-- | Skip a number over chunks starting at its first character.
runSkipNumber :: [Text] -> IO (Either Text Text)
runSkipNumber [] = pure (Left "empty chunk list")
runSkipNumber (c : cs) = case T.uncons c of
  Nothing -> pure (Left "empty first chunk")
  Just (d, _) -> do
    result <- runExceptT $ do
      (remHead, rest) <- skipNumberText (startNumberState d) (T.drop 1 c) (S.each cs)
      remChunks <- S.toList_ rest
      pure (remHead <> T.concat remChunks)
    pure (first renderHQError result)

-- | Collect a number over the same chunks: remainder, captured bytes
-- and estimated size.
runCollectNumber :: [Text] -> IO (Either Text (Text, Text, Int))
runCollectNumber [] = pure (Left "empty chunk list")
runCollectNumber (c : cs) = case T.uncons c of
  Nothing -> pure (Left "empty first chunk")
  Just _ -> do
    result <- runExceptT $ do
      (remHead, rawB, rawS, rest) <- skipNumberCollect c (S.each cs)
      remChunks <- S.toList_ rest
      pure (remHead <> T.concat remChunks, decodeUtf8 (toStrict (toLazyByteString rawB)), rawS)
    pure (first renderHQError result)

collectNumberSpec :: Spec
collectNumberSpec = describe "skipNumberCollect" $ do
  it "captures exactly the bytes it skips across every split" $ do
    forM_ collectNumberCorpus $ \full ->
      forM_ (splits full) $ \chunks -> do
        skipped <- runSkipNumber chunks
        collected <- runCollectNumber chunks
        case (skipped, collected) of
          (Left err1, Left err2) -> err1 `shouldBe` err2
          (Right rem1, Right (rem2, bytes, size)) -> do
            rem1 `shouldBe` rem2
            let total = T.concat chunks
                consumed = T.take (T.length total - T.length rem2) total
            bytes `shouldBe` consumed
            size `shouldBe` T.length consumed
          (a, b) -> expectationFailure $ "mismatch: " <> show (a, b)
  where
    collectNumberCorpus :: [Text]
    collectNumberCorpus =
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
        "01",
        "1e",
        "1e+",
        "-",
        "--1",
        "12a"
      ]
