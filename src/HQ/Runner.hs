{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner (RunnerF (..), Runner, runRunner, runRunnerIO, jsonRunner, runFold) where

import Control.Monad.Error.Class (MonadError (throwError))
import qualified Data.ByteString as BS
import Data.Fix (Fix (..))
import Data.Text.IO (hPutStrLn)
import HQ.JSON.Cursor
import HQ.JSON.Decoder (decodeIO)
import HQ.JSON.Encoder (encode)
import HQ.JSON.Event
import HQ.Optic (Optic (..), OpticF (..))
import HQ.Query (Query (..))
import Relude hiding (Compose)
import Streaming (Of (..), Stream)
import qualified Streaming.Prelude as S

type StreamIO s = Stream (Of s) (ExceptT Text IO)

type ValueStream = StreamIO JSONEvent ()

type K = Cursor -> ExceptT Text IO ValueStream

-- | Interpret an optic against the JSON value at the cursor.
--
-- The continuation receives the cursor positioned immediately after
-- the value currently being interpreted.
runOptic :: Optic -> Cursor -> K -> ExceptT Text IO ValueStream
runOptic (Optic optic) = run optic
  where
    run :: Fix OpticF -> Cursor -> K -> ExceptT Text IO ValueStream
    run (Fix opticF) cursor k = case opticF of
      Id -> k cursor
      Field name -> runField name cursor k
      Each -> runEach cursor k
      Compose left right -> run left cursor $ \selected -> run right selected k
      PrismString -> runScalar isString cursor k
      PrismNumber -> runScalar isNumber cursor k
      PrismBool -> runScalar isBool cursor k
      PrismNull -> runScalar isNull cursor k
      PrismArray -> runContainer isArray cursor k
      PrismObject -> runContainer isObject cursor k
      PrismJust -> runJust cursor k
      Prism1 -> runIndex 0 cursor k
      Prism2 -> runIndex 1 cursor k

    runField :: Text -> Cursor -> K -> ExceptT Text IO ValueStream
    runField name cursor k =
      next cursor >>= \case
        Nothing -> pure mempty
        Just (event, rest) -> case event of
          JSONBeginObject -> findField name rest k
          _ -> pure mempty

    findField :: Text -> Cursor -> K -> ExceptT Text IO ValueStream
    findField name cursor k =
      next cursor >>= \case
        Nothing -> throwError "unexpected end of input while reading object"
        Just (event, rest) ->
          case event of
            JSONEndObject -> pure mempty
            JSONObjectKey key
              -- The cursor now points at the field's value.
              | key == name -> k rest
              -- Skip the value of this unrelated field and continue.
              | otherwise ->
                  skipValue rest >>= \case
                    Nothing -> throwError "unexpected end of input after skipping value"
                    Just afterValue -> findField name afterValue k
            _ -> throwError "invalid JSON object"

    ----------------------------------------------------------------
    -- Each
    ----------------------------------------------------------------

    runEach :: Cursor -> K -> ExceptT Text IO ValueStream
    runEach cursor k =
      next cursor >>= \case
        Nothing -> pure mempty
        Just (event, rest) -> case event of
          JSONBeginArray -> pure (eachArray rest)
          JSONBeginObject -> pure (eachObject rest)
          _ -> pure mempty
      where
        eachArray :: Cursor -> ValueStream
        eachArray c = do
          mNext <- lift (next c)
          case mNext of
            Nothing -> lift $ do
              throwError "unexpected end of input while reading array"
            Just (event, rest) ->
              case event of
                JSONEndArray -> pure ()
                _ -> do
                  let element = fromFirst event rest
                  mConsumed <- lift (consumeValue element)
                  case mConsumed of
                    Nothing -> lift $ throwError "unexpected end of input"
                    Just (events, afterValue) -> do
                      let freshCursor = mkCursorFromList events
                      join $ lift $ k freshCursor
                      eachArray afterValue

        eachObject :: Cursor -> ValueStream
        eachObject c = do
          mNext <- lift (next c)
          case mNext of
            Nothing -> lift $ do
              throwError "unexpected end of input while reading object"
            Just (event, rest) -> case event of
              JSONEndObject -> pure ()
              JSONObjectKey _ -> do
                mVal <- lift (next rest)
                case mVal of
                  Nothing -> lift $ do
                    throwError "unexpected end of input after object key"
                  Just (valueEvent, valueRest) -> do
                    let value = fromFirst valueEvent valueRest
                    mConsumed <- lift (consumeValue value)
                    case mConsumed of
                      Nothing -> lift $ throwError "unexpected end of input"
                      Just (events, afterValue) -> do
                        let freshCursor = mkCursorFromList events
                        join $ lift $ k freshCursor
                        eachObject afterValue
              _ -> lift $ throwError "invalid JSON object"

    ----------------------------------------------------------------
    -- Scalar prisms
    ----------------------------------------------------------------

    runScalar ::
      (JSONEvent -> Bool) ->
      Cursor ->
      K ->
      ExceptT Text IO ValueStream
    runScalar predicate cursor k =
      next cursor >>= \case
        Nothing -> pure mempty
        Just (event, rest)
          | predicate event -> k (fromFirst event rest)
          | otherwise -> pure mempty

    ----------------------------------------------------------------
    -- Container prisms
    ----------------------------------------------------------------

    runContainer :: (JSONEvent -> Bool) -> Cursor -> K -> ExceptT Text IO ValueStream
    runContainer predicate cursor k =
      next cursor >>= \case
        Nothing -> pure mempty
        Just (event, rest)
          | predicate event -> k (fromFirst event rest)
          | otherwise -> pure mempty

    ----------------------------------------------------------------
    -- PrismJust
    ----------------------------------------------------------------

    runJust :: Cursor -> K -> ExceptT Text IO ValueStream
    runJust cursor k =
      next cursor >>= \case
        Nothing -> pure mempty
        Just (JSONNull, _) -> pure mempty
        Just (event, rest) -> k (fromFirst event rest)

    ----------------------------------------------------------------
    -- Array index
    ----------------------------------------------------------------

    runIndex :: Int -> Cursor -> K -> ExceptT Text IO ValueStream
    runIndex index cursor k =
      next cursor >>= \case
        Nothing -> pure mempty
        Just (JSONBeginArray, rest) -> findIndex index rest k
        Just _ -> pure mempty

    findIndex :: Int -> Cursor -> K -> ExceptT Text IO ValueStream
    findIndex index cursor k
      | index < 0 = pure mempty
      | otherwise =
          next cursor >>= \case
            Nothing -> throwError "unexpected end of input while reading array"
            Just (JSONEndArray, _) -> pure mempty
            Just (event, rest)
              | index == 0 -> k (fromFirst event rest)
              | otherwise -> do
                  let value = fromFirst event rest
                  afterValue <- skipValue value
                  case afterValue of
                    Nothing -> throwError "Empty Cursor"
                    Just newCursor -> findIndex (index - 1) newCursor k

    ----------------------------------------------------------------
    -- Cursor helpers
    ----------------------------------------------------------------

    -- \| Construct a cursor whose first event has already been read.
    --
    -- This is the crucial operation for streaming composition:
    -- `event` is not buffered into a list; it becomes the first event
    -- of the new cursor.
    fromFirst :: JSONEvent -> Cursor -> Cursor
    fromFirst event rest = Cursor $ do
      pure (Just (event, rest))

    -- \| Build a cursor from a pre-collected list of events.
    --
    -- The resulting cursor reads from the list, not from the original
    -- stream.  This is safe for multiple readers (e.g., both 'k' and
    -- 'skipValue') because each 'next' call destructures the list
    -- without side effects.
    mkCursorFromList :: [JSONEvent] -> Cursor
    mkCursorFromList [] = Cursor (pure Nothing)
    mkCursorFromList (ev : rest) = Cursor (pure (Just (ev, mkCursorFromList rest)))

    isString :: JSONEvent -> Bool
    isString = \case
      JSONString _ -> True
      _ -> False

    isNumber :: JSONEvent -> Bool
    isNumber = \case
      JSONNumber _ -> True
      _ -> False

    isBool :: JSONEvent -> Bool
    isBool = \case
      JSONBool _ -> True
      _ -> False

    isNull :: JSONEvent -> Bool
    isNull = \case
      JSONNull -> True
      _ -> False

    isArray :: JSONEvent -> Bool
    isArray = \case
      JSONBeginArray -> True
      _ -> False

    isObject :: JSONEvent -> Bool
    isObject = \case
      JSONBeginObject -> True
      _ -> False

-- | Execute an optic and return its selected values.
runFold :: Optic -> Cursor -> ExceptT Text IO ValueStream
runFold optic cursor = runOptic optic cursor emitValue

-- | Emit exactly one JSON value from a cursor.
--
-- This is the one place where the selected value is consumed and
-- forwarded to the output. It does not materialize the value.
emitValue ::
  Cursor ->
  ExceptT Text IO ValueStream
emitValue cursor =
  next cursor >>= \case
    Nothing ->
      pure mempty
    Just (event, rest) ->
      case event of
        JSONNull ->
          pure (S.yield event)
        JSONBool _ ->
          pure (S.yield event)
        JSONNumber _ ->
          pure (S.yield event)
        JSONString _ ->
          pure (S.yield event)
        JSONBeginArray ->
          pure
            $ S.yield event
            <> emitContainer JSONEndArray rest
        JSONBeginObject ->
          pure
            $ S.yield event
            <> emitContainer JSONEndObject rest
        JSONObjectKey _ ->
          throwError "unexpected object key"
        JSONEndArray ->
          throwError "unexpected end of array"
        JSONEndObject ->
          throwError "unexpected end of object"

emitContainer ::
  JSONEvent ->
  Cursor ->
  ValueStream
emitContainer closing cursor = void (emit closing cursor)
  where
    -- Run a container's events, yielding each one and finishing with the
    -- cursor positioned immediately after the container. Nested
    -- containers of either type are handled by recursing, so a nested
    -- closing event never ends the outer container prematurely.
    emit :: JSONEvent -> Cursor -> StreamIO JSONEvent Cursor
    emit closing' c = do
      result <- lift $ next c
      case result of
        Nothing -> throwError "unexpected end of JSON input"
        Just (event, rest)
          | event == closing' -> do
              S.yield event
              pure rest
          | event == JSONBeginArray -> do
              S.yield event
              afterNested <- emit JSONEndArray rest
              emit closing' afterNested
          | event == JSONBeginObject -> do
              S.yield event
              afterNested <- emit JSONEndObject rest
              emit closing' afterNested
          | otherwise -> do
              S.yield event
              emit closing' rest

data RunnerEnv = RunnerEnv
  { runnerEnvQuery :: Query,
    runnerEnvInput :: StreamIO ByteString ()
  }

newtype RunnerF a = Runner {unRunner :: ReaderT RunnerEnv (ExceptT Text IO) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader RunnerEnv,
      MonadError Text
    )

type Runner = RunnerF ValueStream

runRunner :: Runner -> Query -> Handle -> ExceptT Text IO ValueStream
runRunner (Runner runner) query handle = runReaderT runner env
  where
    env = RunnerEnv query $ streamHandle 64 handle

runRunnerIO :: Runner -> Query -> Handle -> IO ()
runRunnerIO runner query handle = do
  hSetBuffering stdout (BlockBuffering Nothing)
  r <- runExceptT $ do
    streamIO <- runRunner runner query handle
    S.mapM_ write $ encode 32 streamIO
  case r of
    Left e -> hPutStrLn stderr e >> exitFailure
    Right v -> pure v
  where
    write = liftIO . BS.hPut stdout

-- | Read strict 'ByteString' chunks from a handle.
-- The input is never loaded into memory as a whole. Each chunk is pulled
-- only when the downstream parser needs more data.
streamHandle :: Int -> Handle -> StreamIO ByteString ()
streamHandle chunkSize' handle = do
  chunk <- liftIO $ BS.hGetSome handle chunkSize'
  if BS.null chunk
    then pure ()
    else S.yield chunk >> streamHandle chunkSize' handle

-- | Execute a query against a JSON value.
executeQuery :: Query -> Cursor -> ExceptT Text IO ValueStream
executeQuery (Preview optic) val = runFold optic val
executeQuery (Fold optic) val = runFold optic val
executeQuery _ _ = throwError "Not supported"

jsonRunner :: Runner
jsonRunner = do
  query <- asks runnerEnvQuery
  input <- asks runnerEnvInput
  let textInput = decodeUtf8Stream input
  let eventStream = decodeIO textInput
  Runner $ lift $ executeQuery query $ fromStream eventStream

decodeUtf8Stream :: StreamIO ByteString () -> StreamIO Text ()
decodeUtf8Stream = go mempty
  where
    go :: ByteString -> StreamIO ByteString () -> StreamIO Text ()
    go leftover stream = do
      result <- lift $ S.next stream
      case result of
        -- Stream exhausted; decode any remaining leftover bytes.
        Left () -> when (not $ BS.null leftover) $ decodeAndYield leftover
        Right (chunk, rest) -> do
          let combined = leftover <> chunk
              safeLen = safePrefixLen combined
              (safe, trailing) = BS.splitAt safeLen combined
          when (not $ BS.null safe) $ decodeAndYield safe
          go trailing rest

    decodeAndYield :: ByteString -> StreamIO Text ()
    decodeAndYield bs = case decodeUtf8' bs of
      Left _ -> throwError "invalid UTF-8 input"
      Right text -> S.yield text

-- | Compute the length of the longest prefix of a strict ByteString that
-- contains only complete UTF-8 sequences.
--
-- Any trailing partial multi-byte sequence is excluded so that the
-- remaining bytes can be carried over and decoded together with the next
-- chunk. Without this, a multi-byte code point straddling a chunk
-- boundary would be decoded as two invalid fragments.
safePrefixLen :: ByteString -> Int
safePrefixLen bs = total - partialTail
  where
    total = BS.length bs
    -- Index of the lead byte of the final sequence, found by stepping
    -- back over at most three trailing continuation bytes.
    leadIdx = seekLead (total - 1) (0 :: Int)
    seekLead i continuations
      | i < 0 = -1
      | continuations > 3 = -1
      | isContinuation (BS.index bs i) = seekLead (i - 1) (continuations + 1)
      | otherwise = i

    -- Number of bytes of the final sequence that are present.
    partialTail
      | total == 0 = 0
      | leadIdx < 0 = 0
      | present >= expected = 0
      | otherwise = present
      where
        lead = BS.index bs leadIdx
        present = total - leadIdx
        expected
          | lead < 0x80 = 1
          | lead < 0xC0 = 1
          | lead < 0xE0 = 2
          | lead < 0xF0 = 3
          | otherwise = 4
    isContinuation b = b >= 0x80 && b < 0xC0
