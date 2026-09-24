{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.JSON.SkipSpec (spec) where

import Control.Monad.Error.Class (throwError)
import qualified Data.Text as T
import HQ.JSON.Decoder
import HQ.JSON.Event (JSONEvent (..))
import HQ.JSON.Skip
import Relude hiding (Compose, id)
import Streaming (Of (..), Stream)
import qualified Streaming.Prelude as S
import Test.HQ (decodeShown, drainCollect, malformedPullCorpus, runStreaming, splits, validPullCorpus)
import Test.Syd

spec :: Spec
spec = describe "HQ.JSON.Skip" $ do
  skipTextSpec
  skipContainerTextSpec

-- | Map a decoded event list to the skip-result shape: errors pass
-- through, successes mean exact consumption.
firstShown :: Either Text [JSONEvent] -> Either Text (Text, [Text])
firstShown (Left err) = Left err
firstShown (Right _) = Right (mempty, [])

--------------------------------------------------------------------------------
-- skipValueText / skipMemberValueText
--------------------------------------------------------------------------------

-- | Skip one value over chunks; collect remainder text and chunks.
runSkipValue :: [Text] -> IO (Either Text (Text, [Text]))
runSkipValue chunks = runExceptT $ case chunks of
  [] -> throwError ("empty chunk list" :: Text)
  c : cs -> do
    (remText, rest) <- skipValueText [] c (S.each cs)
    remaining <- S.toList_ rest
    pure (remText, remaining)

-- | Skip an object member value over chunks starting after the key.
runSkipMember :: [Text] -> IO (Either Text (Text, [Text]))
runSkipMember chunks = runExceptT $ case chunks of
  [] -> throwError ("empty chunk list" :: Text)
  c : cs -> do
    (remText, rest) <- skipMemberValueText [ContextObject] c (S.each cs)
    remaining <- S.toList_ rest
    pure (remText, remaining)

-- | Drain a hand-positioned mid-object decoder: after skipping member
-- @a@, the rest must decode to the remaining members.
drainAfterSkippedMember :: Text -> Either ParseError [JSONEvent]
drainAfterSkippedMember remainder =
  let decoder =
        Decoder
          { decoderInput = remainder,
            decoderStack = [ContextObject],
            decoderState = ParserStateObjectComma
          }
   in case step decoder of
        Left err -> Left err
        Right result -> drainCollect result

skipTextSpec :: Spec
skipTextSpec = describe "skipValueText" $ do
  it "consumes single values exactly under every split" $ do
    forM_ exactPullCorpus $ \input ->
      forM_ (splits input) $ \chunks -> do
        skipped <- runSkipValue chunks
        case skipped of
          Left err -> expectationFailure $ "skip failed: " <> toString err
          Right (remHead, remChunks) ->
            (remHead <> T.concat remChunks) `shouldBe` mempty

  it "leaves trailing whitespace after skipped values" $ do
    forM_ (splits " {\"a\" : [1, 2] } ") $ \chunks -> do
      skipped <- runSkipValue chunks
      case skipped of
        Left err -> expectationFailure $ "skip failed: " <> toString err
        Right (remHead, remChunks) ->
          (remHead <> T.concat remChunks) `shouldBe` " "

  it "reports the same errors as streaming decode under every split" $ do
    forM_ malformedPullCorpus $ \input ->
      forM_ (splits input) $ \chunks -> do
        skipped <- runSkipValue chunks
        skipped `shouldBe` firstShown (decodeShown chunks)

  it "stops after complete values with trailing garbage" $ do
    -- A complete value followed by garbage decodes to an error, but
    -- skipping consumes exactly the value and returns the rest: the
    -- continuation, not the skipper, reports the trailing input.
    let inputJSON =
          [ ("\"a\":1", ":1"),
            ("[1] trailing", " trailing"),
            ("{\"a\":1} x", " x")
          ]
    forM_ inputJSON $ \(input, trailing) ->
      forM_ (splits input) $ \chunks -> do
        skipped <- runSkipValue chunks
        case skipped of
          Left err -> expectationFailure $ "skip failed: " <> toString err
          Right (remHead, remChunks) ->
            (remHead <> T.concat remChunks) `shouldBe` trailing

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
pullRemaining :: Decoder -> Stream (Of Text) (ExceptT Text IO) () -> ExceptT Text IO [JSONEvent]
pullRemaining decoder text = do
  pulled <- pullEvent decoder text
  case pulled of
    PulledEnd -> pure []
    PulledEvent event decoder' rest -> (event :) <$> pullRemaining decoder' rest

skipContainerTextSpec :: Spec
skipContainerTextSpec = describe "skipContainerText" $ do
  it "skips pulled containers and continues decoding after every split" $ do
    forM_ containerValues $ \value ->
      let full = "[" <> value <> ",99]"
       in forM_ (splits full) $ \chunks -> do
            result <- runExceptT $ do
              let textStream :: Stream (Of Text) (ExceptT Text IO) ()
                  textStream = S.each chunks
              openArray <- pullEvent initialDecoder textStream
              case openArray of
                PulledEvent JSONBeginArray decoder1 text1 -> do
                  openMember <- pullEvent decoder1 text1
                  case openMember of
                    PulledEvent open decoder2 text2 -> do
                      (decoder3, text3) <- skipContainerText open decoder2 text2
                      pullRemaining decoder3 text3
                    _ -> throwError ("expected a member opener" :: Text)
                _ -> throwError ("expected an array opener" :: Text)
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

-- | Values without leading/trailing whitespace: skipping must consume
-- everything. (Padded, empty and blank inputs are covered by the
-- neighbouring tests.)
exactPullCorpus :: [Text]
exactPullCorpus = filter (`notElem` ["", "   ", " {\"a\" : [1, 2] } "]) validPullCorpus
