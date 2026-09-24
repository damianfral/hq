{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner where

import Control.Monad.Error.Class (MonadError (throwError))
import Data.Aeson (Value)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Fix (Fix (..))
import Data.Functor.Of (Of ((:>)))
import Data.Text.IO (hPutStrLn)
import HQ.JSON.Decoder
  ( Decoder (..),
    DecoderResult (..),
    ParseError (..),
    ParserState (..),
    Pulled (..),
    StreamIO,
    finish,
    finishValue,
    initialDecoder,
    pullEvent,
  )
import qualified HQ.JSON.Decoder as Decoder
import HQ.JSON.Encoder
  ( BSStream,
    ChunkStream,
    EncodeCtx,
    EncodeStyle (..),
    EncoderConfig (EncoderConfig),
    Join (..),
    Raw (..),
    ValueOptions (..),
    encode,
    encodeChunks,
    formatEvent,
  )
import HQ.JSON.Event
import HQ.JSON.Skip (skipContainerText, skipMemberValueText)
import HQ.Optic (Optic (..), OpticF (..))
import HQ.Query (Query (..))
import HQ.Transformation (Transformation (..), TransformationF (..), runTransformation)
import Relude hiding (Compose, Const)
import qualified Streaming.Prelude as S
import System.IO (hSetBinaryMode)

type ValueStreamF = StreamIO JSONEvent

type ValueStream = ValueStreamF ()

-- | Input cursor: pushed-back events with the decoder and text
-- positioned after them. Navigation peeks at events through
-- 'pullCursor'; bulk take loops decode forward; skipped regions never
-- decode into events at all.
data Cursor = Cursor ![JSONEvent] !Decoder (StreamIO Text ())

-- | Interpret an optic against the JSON value at the cursor,
-- yielding the taken events and returning the advanced cursor.
type K = Cursor -> ValueStreamF Cursor

-- | Splice output: chunk stream returning advanced encoder contexts
-- and cursor. Passthrough regions transcribe text straight to chunks
-- without an intermediate event stream.
type KSplice = Cursor -> [EncodeCtx] -> ChunkStream (ExceptT Text IO) ([EncodeCtx], Cursor)

-- | Pull one event for navigation. Buffered events come first;
-- otherwise the decoder drives forward. Returns 'Nothing' at clean
-- end of input.
pullCursor :: Cursor -> ExceptT Text IO (Maybe (JSONEvent, Cursor))
pullCursor (Cursor (event : buffered) decoder text) =
  pure (Just (event, Cursor buffered decoder text))
pullCursor (Cursor [] decoder text) = do
  pulled <- pullEvent decoder text
  case pulled of
    PulledEnd -> pure Nothing
    PulledEvent event decoder' rest -> pure (Just (event, Cursor [] decoder' rest))

-- | Push an event back for the continuation to see.
pushCursor :: JSONEvent -> Cursor -> Cursor
pushCursor event (Cursor buffered decoder text) = Cursor (event : buffered) decoder text

-- | Pull a single event for bulk takes. Used once per taken value
-- (not per event), so the intermediate tuple is negligible.
pullOne :: [JSONEvent] -> Decoder -> StreamIO Text () -> ExceptT Text IO (JSONEvent, [JSONEvent], Decoder, StreamIO Text ())
pullOne (event : buffered) decoder text = pure (event, buffered, decoder, text)
pullOne [] decoder text = do
  pulled <- pullEvent decoder text
  case pulled of
    PulledEnd -> throwError "unexpected end of JSON input"
    PulledEvent event decoder' rest -> pure (event, [], decoder', rest)

-- | Interpret an optic against the JSON value at the cursor.
--
-- The continuation receives the cursor positioned immediately after
-- the value currently being interpreted.
runFold :: Optic -> K
runFold (Optic optic) = run optic takeValue
  where
    run :: Fix OpticF -> K -> K
    run (Fix opticF) k input = case opticF of
      Id -> k input
      Field name -> runField name k input
      Each -> runEach k input
      Compose left right -> run left (run right k) input
      PrismString -> runScalar isString k input
      PrismNumber -> runScalar isNumber k input
      PrismBool -> runScalar isBool k input
      PrismNull -> runScalar isNull k input
      PrismArray -> runScalar isArray k input
      PrismObject -> runScalar isObject k input
      PrismJust -> runJust k input
      Prism1 -> runIndex 0 k input
      Prism2 -> runIndex 1 k input
      Ix i -> runIndex i k input

    runField :: Text -> K -> K
    runField name k input = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure input
        Just (JSONBeginObject, rest) -> findField name k rest
        Just (event, rest) -> skipValue (pushCursor event rest)

    findField :: Text -> K -> K
    findField name k = go
      where
        go input = do
          result <- lift (pullCursor input)
          case result of
            Nothing -> throwError "unexpected end of input while reading object"
            Just (JSONEndObject, rest) -> pure rest
            Just (JSONObjectKey key, rest)
              | key == name -> do
                  afterField <- k rest
                  skipRestOfObject afterField
              | otherwise -> do
                  afterValue <- skipMemberValue rest
                  go afterValue
            Just _ -> throwError "invalid JSON object"

    -- \| Consume the rest of an object after a matched member value.
    skipRestOfObject :: Cursor -> ValueStreamF Cursor
    skipRestOfObject input = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> throwError "unexpected end of input while reading object"
        Just (JSONEndObject, rest) -> pure rest
        Just (JSONObjectKey _, rest) -> do
          afterValue <- skipMemberValue rest
          skipRestOfObject afterValue
        Just _ -> throwError "invalid JSON object"

    ----------------------------------------------------------------
    -- Each
    ----------------------------------------------------------------

    runEach :: K -> K
    runEach k input = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure input
        Just (JSONBeginArray, rest) -> eachArray rest
        Just (JSONBeginObject, rest) -> eachObject rest
        Just (event, rest) -> skipValue (pushCursor event rest)
      where
        eachArray stream = do
          result <- lift (pullCursor stream)
          case result of
            Nothing -> throwError "unexpected end of input while reading array"
            Just (JSONEndArray, rest) -> pure rest
            Just (event, rest) -> do
              afterElement <- k (pushCursor event rest)
              eachArray afterElement

        eachObject stream = do
          result <- lift (pullCursor stream)
          case result of
            Nothing -> throwError "unexpected end of input while reading object"
            Just (JSONEndObject, rest) -> pure rest
            Just (JSONObjectKey _, rest) -> do
              afterValue <- k rest -- cursor is already at the value
              eachObject afterValue
            Just _ -> throwError "invalid JSON object"

    ----------------------------------------------------------------
    -- Scalar prisms
    ----------------------------------------------------------------

    runScalar :: (JSONEvent -> Bool) -> K -> K
    runScalar predicate k input = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure input
        Just (event, rest)
          | predicate event -> k (pushCursor event rest)
          | otherwise -> skipValue (pushCursor event rest) -- not a match; consume anyway

    ----------------------------------------------------------------
    -- PrismJust
    ----------------------------------------------------------------

    runJust :: K -> K
    runJust k input = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure input
        Just (JSONNull, rest) -> pure rest
        Just (event, rest) -> k (pushCursor event rest)

    ----------------------------------------------------------------
    -- Array index
    ----------------------------------------------------------------

    runIndex :: Int -> K -> K
    runIndex index k input = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure input
        Just (JSONBeginArray, rest) -> findIndex index rest
        Just (event, rest) -> skipValue (pushCursor event rest)
      where
        findIndex n stream
          | n < 0 = skipRestOfArray stream
          | otherwise = do
              result <- lift (pullCursor stream)
              case result of
                Nothing -> throwError "unexpected end of input while reading array"
                Just (JSONEndArray, rest) -> pure rest
                Just (event, rest) -> case n of
                  0 -> do
                    afterField <- k (pushCursor event rest)
                    skipRestOfArray afterField
                  _ -> do
                    afterValue <- skipValue (pushCursor event rest)
                    findIndex (n - 1) afterValue

    -- \| Consume the rest of an array after a matched element.
    skipRestOfArray :: Cursor -> ValueStreamF Cursor
    skipRestOfArray input = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> throwError "unexpected end of input while reading array"
        Just (JSONEndArray, rest) -> pure rest
        Just (event, rest) -> do
          afterValue <- skipValue (pushCursor event rest)
          skipRestOfArray afterValue

----------------------------------------------------------------
-- Container prisms
----------------------------------------------------------------

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

-- | Execute an optic, emitting at most one value: the first one it
-- selects (lens @preview@ semantics).
--
-- The fold is short-circuited: once the first value has been emitted,
-- no further input is read from the source.
runPreview :: Optic -> K
runPreview optic input = do
  takeFirstValue (runFold optic input)
  pure input -- the after-cursor is meaningless for preview; nobody reads it

--------------------------------------------------------------------------------
-- Over and delete: document rewriting
--------------------------------------------------------------------------------

-- | What happens to a value focused by an optic: it is removed entirely
-- (@delete@) or rewritten by a transformation (@over@, including @set@,
-- which is @over@ with a constant).
data Splicer = SpliceDelete | SpliceTransform Transformation
  deriving (Show, Eq)

-- | Remove every value the optic focuses on from the document, passing
-- the rest of the document through unchanged (lens @delete@/@omit@).
runDelete :: Optic -> EncoderConfig -> KSplice
runDelete = runSplice SpliceDelete

-- | Apply a transformation to every value the optic focuses on, rewriting
-- the document in place (lens @over@).
runOver :: Optic -> Transformation -> EncoderConfig -> KSplice
runOver optic transformation = runSplice (SpliceTransform transformation) optic

-- | Rewrite the document at the cursor by splicing every value that the
-- optic focuses on.
--
-- This is the streaming counterpart to 'runOptic': instead of
-- extracting the focused values, the @rewrite@ family re-emits the
-- document, replacing or omitting the focused values in place.  All
-- other events pass through unchanged, so the output is the input with
-- only the targeted values modified.
runSplice :: Splicer -> Optic -> EncoderConfig -> KSplice
runSplice splicer (Optic optic) config = run optic
  where
    run :: Fix OpticF -> KSplice
    run step input ctxs = case unFix step of
      Id -> spliceValue splicer input ctxs
      _ -> navigate step (Fix Id) input ctxs

    navigate :: Fix OpticF -> Fix OpticF -> KSplice
    navigate step suffix input ctxs = case unFix step of
      Id -> run suffix input ctxs
      Compose left right -> navigate left (composeStep right suffix) input ctxs
      Field name -> rewriteField name suffix input ctxs
      Each -> rewriteEach suffix input ctxs
      PrismString -> rewritePrism isString suffix input ctxs
      PrismNumber -> rewritePrism isNumber suffix input ctxs
      PrismBool -> rewritePrism isBool suffix input ctxs
      PrismNull -> rewritePrism isNull suffix input ctxs
      PrismArray -> rewritePrism isArray suffix input ctxs
      PrismObject -> rewritePrism isObject suffix input ctxs
      PrismJust -> rewriteJust suffix input ctxs
      Prism1 -> rewriteIndex 0 suffix input ctxs
      Prism2 -> rewriteIndex 1 suffix input ctxs
      Ix i -> rewriteIndex i suffix input ctxs

    composeStep :: Fix OpticF -> Fix OpticF -> Fix OpticF
    composeStep (Fix Id) rest = rest
    composeStep step rest = Fix (Compose step rest)

    spliceValue :: Splicer -> KSplice
    spliceValue SpliceDelete input ctxs = do
      after <- lift (skipValueE input)
      pure (ctxs, after)
    -- A constant replacement never reads the focused value: skip it
    -- at the text level instead of decoding it.
    spliceValue (SpliceTransform (Transformation (Fix (Const value)))) input ctxs = do
      after <- lift (skipValueE input)
      emitValueChunks value ctxs after
    spliceValue (SpliceTransform t) input ctxs = do
      events :> rest <- lift (S.toList (takeValue input))
      case eventsToValue events >>= runTransformation t of
        Left err -> throwError err
        Right value -> emitValueChunks value ctxs rest

    -- \| Emit a transformed value's events as chunks.
    emitValueChunks :: Value -> [EncodeCtx] -> Cursor -> ChunkStream (ExceptT Text IO) ([EncodeCtx], Cursor)
    emitValueChunks value ctxs cursor = go ctxs (valueToEvents value)
      where
        go cs [] = pure (cs, cursor)
        go cs (event : events) = do
          cs' <- emitChunk config event cs
          go cs' events

    landing :: Fix OpticF -> JSONEvent -> Bool
    landing (Fix opticF) event = case opticF of
      Id -> True
      Field _ -> False
      Each -> False
      Ix _ -> False
      Prism1 -> False
      Prism2 -> False
      PrismString -> isString event
      PrismNumber -> isNumber event
      PrismBool -> isBool event
      PrismNull -> isNull event
      PrismArray -> isArray event
      PrismObject -> isObject event
      PrismJust -> not (isNull event)
      Compose l r -> landing l event && landing r event

    -- \| Prism: value whose first event satisfies the predicate goes
    -- through the suffix; everything else passes through unchanged.
    rewritePrism :: (JSONEvent -> Bool) -> Fix OpticF -> KSplice
    rewritePrism predicate suffix input ctxs = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure (ctxs, input)
        Just (event, rest)
          | predicate event -> run suffix (pushCursor event rest) ctxs
          | otherwise -> takeValueChunks config (pushCursor event rest) ctxs

    -- \| Prism on non-null: null passes through, anything else goes
    -- through the suffix.
    rewriteJust :: Fix OpticF -> KSplice
    rewriteJust suffix input ctxs = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure (ctxs, input)
        Just (JSONNull, rest) -> do
          ctxs' <- emitChunk config JSONNull ctxs
          pure (ctxs', rest)
        Just (event, rest) -> run suffix (pushCursor event rest) ctxs

    -- \| Array index: splice the element at @index@, pass through the
    -- rest of the array. Non-arrays pass through unchanged.
    rewriteIndex :: Int -> Fix OpticF -> KSplice
    rewriteIndex index suffix input ctxs = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure (ctxs, input)
        Just (JSONBeginArray, rest) -> do
          ctxs' <- emitChunk config JSONBeginArray ctxs
          go index rest ctxs'
        Just (event, rest) -> takeValueChunks config (pushCursor event rest) ctxs
      where
        go n stream ctx = do
          result <- lift (pullCursor stream)
          case result of
            Nothing -> throwError "unexpected end of input while reading array"
            Just (JSONEndArray, rest) -> do
              ctx' <- emitChunk config JSONEndArray ctx
              pure (ctx', rest)
            Just (event, rest)
              | n == 0 -> do
                  (ctx', after) <- run suffix (pushCursor event rest) ctx
                  go (-1) after ctx'
              | otherwise -> do
                  (ctx', after) <- takeValueChunks config (pushCursor event rest) ctx
                  go (n - 1) after ctx'

    -- \| Object field: splice the member named @name@ (dropping the key
    -- too under @delete@ when the suffix lands on the value), pass
    -- every other member through unchanged.
    rewriteField :: Text -> Fix OpticF -> KSplice
    rewriteField name suffix input ctxs = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure (ctxs, input)
        Just (JSONBeginObject, rest) -> do
          ctxs' <- emitChunk config JSONBeginObject ctxs
          pairs rest ctxs'
        Just (event, rest) -> takeValueChunks config (pushCursor event rest) ctxs
      where
        pairs :: Cursor -> [EncodeCtx] -> ChunkStream (ExceptT Text IO) ([EncodeCtx], Cursor)
        pairs stream ctx = do
          result <- lift (pullCursor stream)
          case result of
            Nothing -> throwError "unexpected end of input while reading object"
            Just (JSONEndObject, rest) -> do
              ctx' <- emitChunk config JSONEndObject ctx
              pure (ctx', rest)
            Just (JSONObjectKey key, rest)
              | key == name -> spliceMember suffix key rest pairs ctx
              | otherwise -> do
                  ctx' <- emitChunk config (JSONObjectKey key) ctx
                  (ctx'', after) <- takeValueChunks config rest ctx'
                  pairs after ctx''
            Just _ -> throwError "invalid JSON object"

    -- \| The @each@ traversal: splice every array element, or every
    -- object member value. Non-containers pass through.
    rewriteEach :: Fix OpticF -> KSplice
    rewriteEach suffix input ctxs = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure (ctxs, input)
        Just (JSONBeginArray, rest) -> do
          ctxs' <- emitChunk config JSONBeginArray ctxs
          allElements rest ctxs'
        Just (JSONBeginObject, rest) -> do
          ctxs' <- emitChunk config JSONBeginObject ctxs
          allMembers rest ctxs'
        Just (event, rest) -> takeValueChunks config (pushCursor event rest) ctxs
      where
        allElements :: Cursor -> [EncodeCtx] -> ChunkStream (ExceptT Text IO) ([EncodeCtx], Cursor)
        allElements stream ctx = do
          result <- lift (pullCursor stream)
          case result of
            Nothing -> throwError "unexpected end of input while reading array"
            Just (JSONEndArray, rest) -> do
              ctx' <- emitChunk config JSONEndArray ctx
              pure (ctx', rest)
            Just (event, rest) -> do
              (ctx', after) <- run suffix (pushCursor event rest) ctx
              allElements after ctx'

        allMembers :: Cursor -> [EncodeCtx] -> ChunkStream (ExceptT Text IO) ([EncodeCtx], Cursor)
        allMembers stream ctx = do
          result <- lift (pullCursor stream)
          case result of
            Nothing -> throwError "unexpected end of input while reading object"
            Just (JSONEndObject, rest) -> do
              ctx' <- emitChunk config JSONEndObject ctx
              pure (ctx', rest)
            Just (JSONObjectKey key, rest) ->
              spliceMember suffix key rest allMembers ctx
            Just _ -> throwError "invalid JSON object"

    -- \| Splice one object member. The key survives unless a @delete@
    -- lands on the member value as a whole; the walk then continues
    -- with @continue@.
    spliceMember :: Fix OpticF -> Text -> Cursor -> KSplice -> [EncodeCtx] -> ChunkStream (ExceptT Text IO) ([EncodeCtx], Cursor)
    spliceMember suffix key stream continue ctxs = do
      result <- lift (pullCursor stream)
      case result of
        Nothing -> throwError "unexpected end of input after object key"
        Just (event, rest) -> case splicer of
          SpliceDelete
            | landing suffix event -> do
                after <- lift (skipValueE (pushCursor event rest))
                continue after ctxs
            | otherwise -> do
                ctx' <- emitChunk config (JSONObjectKey key) ctxs
                (ctx'', after) <- run suffix (pushCursor event rest) ctx'
                continue after ctx''
          SpliceTransform _ -> do
            ctx' <- emitChunk config (JSONObjectKey key) ctxs
            (ctx'', after) <- run suffix (pushCursor event rest) ctx'
            continue after ctx''

data RunnerEnv = RunnerEnv
  { runnerEnvQuery :: Query,
    runnerEnvInput :: StreamIO ByteString (),
    runnerEnvConfig :: EncoderConfig
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

type Runner = RunnerF (BSStream (ExceptT Text IO) ())

runRunner :: Runner -> Query -> EncoderConfig -> Handle -> ExceptT Text IO (BSStream (ExceptT Text IO) ())
runRunner (Runner runner) query config handle = runReaderT runner env
  where
    env = RunnerEnv query (streamHandle 256 handle) config

-- | Run a query, encoding the selected values to stdout with pretty
-- formatting and default value options.
runRunnerIO :: Runner -> Query -> Handle -> IO ()
runRunnerIO runner query = runRunnerIOWith runner query config
  where
    config = EncoderConfig (Pretty 2) (ValueOptions NoRaw NoJoin)

-- | Run a query, encoding the selected values to stdout with the given
-- style and value options.
--
-- The stream is written to stdout; errors are reported on stderr with
-- a failing exit status.
runRunnerIOWith :: Runner -> Query -> EncoderConfig -> Handle -> IO ()
runRunnerIOWith runner query encConfig handle = do
  hSetBuffering stdout $ BlockBuffering Nothing
  hSetBinaryMode stdout True
  r <- runExceptT $ do
    byteStream <- runRunner runner query encConfig handle
    S.mapM_ write byteStream
  case r of
    Left e -> hPutStrLn stderr e >> exitFailure
    Right v -> pure v
  where
    write = liftIO . LBS.hPut stdout

-- | Read strict 'ByteString' chunks from a handle.
-- The input is never loaded into memory as a whole. Each chunk is pulled
-- only when the downstream parser needs more data.
--
-- NOTE: the 256-byte size is deliberate. Larger input chunks allocate
-- slightly less overall but run slower per byte (measured): the
-- decoder works better on small, cache-resident texts.
streamHandle :: Int -> Handle -> StreamIO ByteString ()
streamHandle size handle = do
  chunk <- liftIO $ BS.hGetSome handle size
  if BS.null chunk
    then pure ()
    else S.yield chunk >> streamHandle size handle

jsonRunner :: Runner
jsonRunner = do
  query <- asks runnerEnvQuery
  input <- asks runnerEnvInput
  config <- asks runnerEnvConfig
  let cursor = Cursor [] initialDecoder (decodeUtf8Stream input)
  case query of
    Preview optic -> pure (void (encode config 65536 (takeFirstValue (runFold optic cursor))))
    Fold optic -> pure (void (encode config 65536 (runFold optic cursor)))
    Over optic transformation ->
      pure (void (encodeChunks 65536 (runSplice (SpliceTransform transformation) optic config cursor [])))
    Delete optic ->
      pure (void (encodeChunks 65536 (runSplice SpliceDelete optic config cursor [])))

decodeUtf8Stream :: StreamIO ByteString () -> StreamIO Text ()
decodeUtf8Stream = go mempty
  where
    go :: ByteString -> StreamIO ByteString () -> StreamIO Text ()
    go leftover stream = do
      result <- lift $ S.next stream
      case result of
        Left () -> when (leftover /= mempty) $ decodeAndYield leftover
        Right (chunk, rest) -> do
          let combined = leftover <> chunk
              safeLen = safePrefixLen combined
              (safe, trailing) = BS.splitAt safeLen combined
          when (safe /= mempty) $ decodeAndYield safe
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

-- | Stream the events of exactly one complete JSON value, returning the
-- cursor positioned immediately after it. Lazy: a consumer that stops
-- early (e.g. @S.take 1@) pulls only what it needs.
takeValue :: K
takeValue (Cursor buffered decoder text) = do
  (event, buffered', decoder', text') <- lift (pullOne buffered decoder text)
  S.yield event
  case event of
    JSONBeginArray -> takeContainerFrom JSONEndArray buffered' decoder' text'
    JSONBeginObject -> takeContainerFrom JSONEndObject buffered' decoder' text'
    JSONEndArray -> throwError "unexpected end of array"
    JSONEndObject -> throwError "unexpected end of object"
    JSONObjectKey _ -> throwError "unexpected object key"
    _ -> pure (Cursor buffered' decoder' text')

takeFirstValue :: ValueStreamF r -> ValueStream
takeFirstValue = go (0 :: Int)
  where
    go depth stream = do
      result <- lift (S.next stream)
      case result of
        Left _ -> pure ()
        Right (event, rest) -> do
          S.yield event
          case event of
            JSONBeginArray -> go (depth + 1) rest
            JSONBeginObject -> go (depth + 1) rest
            JSONEndArray
              | depth == 1 -> pure ()
              | otherwise -> go (depth - 1) rest
            JSONEndObject
              | depth == 1 -> pure ()
              | otherwise -> go (depth - 1) rest
            _
              | depth == 0 -> pure ()
              | otherwise -> go depth rest

takeContainer :: JSONEvent -> Cursor -> ValueStreamF Cursor
takeContainer closing (Cursor buffered decoder text) =
  takeContainerFrom closing buffered decoder text

-- | Take a container body event by event, driving the decoder
-- directly with no tuples or wrappers on the hot path: the event,
-- buffer, decoder and text flow as explicit arguments.
takeContainerFrom :: JSONEvent -> [JSONEvent] -> Decoder -> StreamIO Text () -> ValueStreamF Cursor
takeContainerFrom closing = go
  where
    go :: [JSONEvent] -> Decoder -> StreamIO Text () -> ValueStreamF Cursor
    go buf dec txt = case buf of
      event : rest -> emit event rest dec txt
      [] -> case Decoder.step dec of
        Left err -> throwError (show err)
        Right (Emit event dec') -> emit event [] dec' txt
        Right (NeedInput dec') -> pullMore dec' txt
        Right (Done _) -> throwError "unexpected end of JSON input"
    emit event buf dec txt = do
      S.yield event
      if event == closing
        then pure (Cursor buf dec txt)
        else case event of
          JSONBeginArray -> nested JSONEndArray buf dec txt
          JSONBeginObject -> nested JSONEndObject buf dec txt
          _ -> go buf dec txt
    nested end buf dec txt = do
      Cursor buf' dec' txt' <- takeContainerFrom end buf dec txt
      takeContainerFrom closing buf' dec' txt'
    pullMore dec txt = do
      result <- lift (S.next txt)
      case result of
        Left () -> finishTake dec
        Right (chunk, rest) -> case Decoder.feed chunk dec of
          Left err -> throwError (show err)
          Right (Emit event dec') -> emit event [] dec' rest
          Right (NeedInput dec') -> pullMore dec' rest
          Right (Done _) -> throwError "unexpected end of JSON input"
    -- Mirror 'drainFinish': a value completed exactly at end of input
    -- still yields its final event; anything else ends the take the
    -- same way the event-stream takes did.
    finishTake dec = case finish dec of
      Left UnexpectedEnd
        | decoderState dec == ParserStateValue && null (decoderStack dec) ->
            throwError "unexpected end of JSON input"
      Left err -> throwError (show err)
      Right (Done _) -> throwError "unexpected end of JSON input"
      Right (NeedInput _) -> throwError (show UnexpectedEnd)
      Right (Emit event dec') -> emit event [] dec' (pure ())

-- | Format one event and yield its chunk, returning advanced contexts.
emitChunk :: EncoderConfig -> JSONEvent -> [EncodeCtx] -> ChunkStream (ExceptT Text IO) [EncodeCtx]
emitChunk config event ctxs = do
  let (chunk, ctxs') = formatEvent config ctxs event
  S.yield chunk
  pure ctxs'

-- | Take one complete value, transcribing it straight to chunks.
takeValueChunks :: EncoderConfig -> Cursor -> [EncodeCtx] -> ChunkStream (ExceptT Text IO) ([EncodeCtx], Cursor)
takeValueChunks config (Cursor buffered decoder text) ctxs = do
  (event, buffered', decoder', text') <- lift (pullOne buffered decoder text)
  ctxs' <- emitChunk config event ctxs
  case event of
    JSONBeginArray -> takeContainerChunks config JSONEndArray buffered' decoder' text' ctxs'
    JSONBeginObject -> takeContainerChunks config JSONEndObject buffered' decoder' text' ctxs'
    JSONEndArray -> throwError "unexpected end of array"
    JSONEndObject -> throwError "unexpected end of object"
    JSONObjectKey _ -> throwError "unexpected object key"
    _ -> pure (ctxs', Cursor buffered' decoder' text')

-- | Take a container body, transcribing text straight to chunks: the
-- decode and format steps fuse per event with no intermediate event
-- stream.
takeContainerChunks :: EncoderConfig -> JSONEvent -> [JSONEvent] -> Decoder -> StreamIO Text () -> [EncodeCtx] -> ChunkStream (ExceptT Text IO) ([EncodeCtx], Cursor)
takeContainerChunks config closing = go
  where
    go buf dec txt ctx = case buf of
      event : rest -> emit event rest dec txt ctx
      [] -> case Decoder.step dec of
        Left err -> throwError (show err)
        Right (Emit event dec') -> emit event [] dec' txt ctx
        Right (NeedInput dec') -> pullMore dec' txt ctx
        Right (Done _) -> throwError "unexpected end of JSON input"
    emit event buf dec txt ctx = do
      ctx' <- emitChunk config event ctx
      if event == closing
        then pure (ctx', Cursor buf dec txt)
        else case event of
          JSONBeginArray -> nested JSONEndArray buf dec txt ctx'
          JSONBeginObject -> nested JSONEndObject buf dec txt ctx'
          _ -> go buf dec txt ctx'
    nested end buf dec txt ctx = do
      (ctx', Cursor buf' dec' txt') <- takeContainerChunks config end buf dec txt ctx
      takeContainerChunks config closing buf' dec' txt' ctx'
    pullMore dec txt ctx = do
      result <- lift (S.next txt)
      case result of
        Left () -> finishTake dec ctx
        Right (chunk, rest) -> case Decoder.feed chunk dec of
          Left err -> throwError (show err)
          Right (Emit event dec') -> emit event [] dec' rest ctx
          Right (NeedInput dec') -> pullMore dec' rest ctx
          Right (Done _) -> throwError "unexpected end of JSON input"
    -- Mirror 'drainFinish', emitting the final event as a chunk.
    finishTake dec ctx = case finish dec of
      Left UnexpectedEnd
        | decoderState dec == ParserStateValue && null (decoderStack dec) ->
            throwError "unexpected end of JSON input"
      Left err -> throwError (show err)
      Right (Done _) -> throwError "unexpected end of JSON input"
      Right (NeedInput _) -> throwError (show UnexpectedEnd)
      Right (Emit event dec') -> emit event [] dec' (pure ()) ctx

-- | Consume one complete value without yielding its events.
--
-- The value's first event is pulled to dispatch on, then containers
-- are skipped at the text level ('skipContainerText') without
-- decoding their contents.
skipValue :: Cursor -> ValueStreamF Cursor
skipValue = lift . skipValueE

-- | 'skipValue' in 'ExceptT': shared by the event-stream and
-- chunk-stream pipelines.
skipValueE :: Cursor -> ExceptT Text IO Cursor
skipValueE input = do
  result <- pullCursor input
  case result of
    Nothing -> throwError "unexpected end of JSON input"
    Just (event, Cursor buffered decoder text) -> skipEvent event buffered decoder text
  where
    skipEvent event buffered decoder text = case event of
      JSONBeginArray -> skipOpened event decoder text buffered
      JSONBeginObject -> skipOpened event decoder text buffered
      JSONEndArray -> throwError "unexpected end of array"
      JSONEndObject -> throwError "unexpected end of object"
      JSONObjectKey _ -> throwError "unexpected object key"
      _ -> pure (Cursor buffered decoder text)
    skipOpened event decoder text buffered = do
      (decoder', rest) <- skipContainerText event decoder text
      pure (Cursor buffered decoder' rest)

-- | Skip an object member value starting right after its key: the
-- colon and value are consumed at the text level.
skipMemberValue :: Cursor -> ValueStreamF Cursor
skipMemberValue = lift . skipMemberValueE

-- | 'skipMemberValue' in 'ExceptT': shared by both pipelines.
skipMemberValueE :: Cursor -> ExceptT Text IO Cursor
skipMemberValueE (Cursor buffered decoder text)
  | null buffered = do
      (remainder, rest) <- skipMemberValueText (decoderStack decoder) (decoderInput decoder) text
      pure (Cursor [] (finishValue decoder {decoderInput = remainder}) rest)
  | otherwise = skipValueE (Cursor buffered decoder text)
