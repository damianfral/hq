{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.JSON.CursorSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import HQ.JSON.Cursor (fromStream)
import HQ.JSON.Decoder (decode, decodeIO)
import HQ.JSON.Event (JSONEvent (..))
import HQ.Optic.Parser (parseOptic)
import HQ.Runner (runFold)
import Relude hiding (Compose, id)
import Streaming (Of (..))
import qualified Streaming.Prelude as S
import Test.Syd

-- | Split text into fixed-size chunks.
chunkText :: Int -> Text -> [Text]
chunkText _ "" = []
chunkText n t =
  let (chunk, rest) = T.splitAt n t
   in chunk : chunkText n rest

-- | Decode text in chunks and collect events.
decodeChunks :: Int -> Text -> Either Text [JSONEvent]
decodeChunks chunkSize input = runIdentity $ do
  let chunks = chunkText chunkSize input
  result <- S.toList (decode (S.each chunks))
  case result of
    events :> Right _ -> pure (Right events)
    _ :> Left err -> pure (Left (show err))

-- | Run a fold optic through the full streaming pipeline with chunked text input.
runFoldChunked :: Text -> Text -> Int -> IO (Either Text [JSONEvent])
runFoldChunked opticStr jsonInput chunkSize =
  case parseOptic opticStr of
    Left err -> pure (Left (show err))
    Right optic -> runExceptT $ do
      let chunks = chunkText chunkSize jsonInput
          cursor = fromStream (decodeIO (S.each chunks))
      resultStream <- runFold optic cursor
      events <- S.toList resultStream
      case events of
        evts :> _ -> pure evts

spec :: Spec
spec = describe "HQ.JSON.Cursor" $ do
  it "streaming decode handles 5MB.json in chunks" $ do
    contents <- liftIO $ BS.readFile "test/test-resources/5MB.json"
    let text = decodeUtf8 contents
        events = decodeChunks 64 text
    case events of
      Left err -> expectationFailure $ "Stream decode failed: " <> toString err
      Right evts -> do
        length evts `shouldSatisfy` (> 1000)
        viaNonEmpty head evts `shouldBe` Just JSONBeginArray

  it "fold each.#name works on small input" $ do
    let text = "[{\"name\":\"alice\"},{\"name\":\"bob\"}]"
    result <- runFoldChunked "each . #name" text 10
    case result of
      Left err -> expectationFailure $ "Fold failed: " <> toString err
      Right evts ->
        evts `shouldBe` [JSONString "alice", JSONString "bob"]

  it "fold each.#name works on 5MB.json in 64-char chunks" $ do
    contents <- liftIO $ BS.readFile "test/test-resources/5MB.json"
    let text = decodeUtf8 contents
    result <- runFoldChunked "each . #name" text 64
    case result of
      Left err -> expectationFailure $ "Fold failed: " <> toString err
      Right evts -> do
        length evts `shouldSatisfy` (> 100)
        forM_ evts $ \case
          JSONString _ -> pure ()
          other -> expectationFailure $ "Expected JSONString, got: " <> show other

  xit "fold each.#created_at works on large-file.json in 64-byte chunks" $ do
    contents <- liftIO $ BS.readFile "test/test-resources/large-file.json"
    let text = decodeUtf8 contents
    result <- runFoldChunked "each . #created_at" text 64
    case result of
      Left err -> expectationFailure $ "Fold failed: " <> toString err
      Right evts -> do
        length evts `shouldSatisfy` (> 100)
        forM_ evts $ \case
          JSONString _ -> pure ()
          other -> expectationFailure $ "Expected JSONString, got: " <> show other

  it "fold id emits complete nested containers" $ do
    let text = "[{\"a\":{\"b\":[1,2,{\"c\":3}]},\"d\":4},[5,[6]]]"
    result <- runFoldChunked "id" text 3
    case result of
      Left err -> expectationFailure $ "Fold failed: " <> toString err
      Right evts -> do
        evts `shouldBe` [JSONBeginArray, JSONBeginObject, JSONObjectKey "a", JSONBeginObject, JSONObjectKey "b", JSONBeginArray, JSONNumber 1, JSONNumber 2, JSONBeginObject, JSONObjectKey "c", JSONNumber 3, JSONEndObject, JSONEndArray, JSONEndObject, JSONObjectKey "d", JSONNumber 4, JSONEndObject, JSONBeginArray, JSONNumber 5, JSONBeginArray, JSONNumber 6, JSONEndArray, JSONEndArray, JSONEndArray]

  it "streaming decode matches complete decode for small input" $ do
    let input = "[{\"name\":\"alice\"},{\"name\":\"bob\"}]"
        streamed = decodeChunks 10 input
        complete = decodeChunks (T.length input) input
    streamed `shouldBe` complete

  it "streaming decode handles string spanning chunk boundary" $ do
    let input = "{\"key\":\"hello world\"}"
        streamed = decodeChunks 10 input
    case streamed of
      Left err -> expectationFailure $ "Failed: " <> toString err
      Right evts ->
        evts
          `shouldBe` [ JSONBeginObject,
                       JSONObjectKey "key",
                       JSONString "hello world",
                       JSONEndObject
                     ]

  it "streaming decode handles multiple strings across boundaries" $ do
    let input = "[\"abc\",\"def\",\"ghi\"]"
        streamed = decodeChunks 5 input
    case streamed of
      Left err -> expectationFailure $ "Failed: " <> toString err
      Right evts ->
        evts
          `shouldBe` [ JSONBeginArray,
                       JSONString "abc",
                       JSONString "def",
                       JSONString "ghi",
                       JSONEndArray
                     ]
