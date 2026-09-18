{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner (RunnerF (..), Runner, runRunner, runRunnerIO, runRunnerIOWith, jsonRunner, ValueStream, runFold, runPreview, runSet, runDelete, runOver) where

import Control.Monad.Error.Class (MonadError (throwError))
import qualified Data.ByteString as BS
import Data.Fix (Fix (..))
import Data.Text.IO (hPutStrLn)
import HQ.JSON.Cursor
import HQ.JSON.Decoder (StreamIO, decodeIO)
import HQ.JSON.Encoder (EncodeStyle (..), EncoderConfig (EncoderConfig), Join (..), Raw (..), ValueOptions (..), encode)
import HQ.JSON.Event
import HQ.Optic (Optic (..), OpticF (..))
import HQ.Query (Query (..))
import HQ.Transformation (Transformation, runTransformation)
import Relude hiding (Compose)
import qualified Streaming.Prelude as S

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
      Ix i -> runIndex i cursor k

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

-- | Construct a cursor whose first event has already been read.
--
-- This is the crucial operation for streaming composition:
-- @event@ is not buffered into a list; it becomes the first event
-- of the new cursor.
fromFirst :: JSONEvent -> Cursor -> Cursor
fromFirst event rest = Cursor $ do
  pure (Just (event, rest))

-- | Build a cursor from a pre-collected list of events.
--
-- The resulting cursor reads from the list, not from the original
-- stream.  This is safe for multiple readers (e.g., the fold and
-- rewrite interpreters) because each 'next' call destructures the list
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

-- | Execute an optic, emitting at most one value: the first one it
-- selects (lens @preview@ semantics).
--
-- The fold is short-circuited: once the first value has been emitted,
-- no further input is read from the source.
runPreview :: Optic -> Cursor -> ExceptT Text IO ValueStream
runPreview optic cursor = do
  values <- runOptic optic cursor emitValue
  pure (firstValue values)

-- | Keep only the first complete top-level value of a value stream and
-- drop the rest without consuming it.
firstValue :: ValueStream -> ValueStream
firstValue = go 0
  where
    go :: Int -> ValueStream -> ValueStream
    go depth stream = do
      result <- lift $ S.next stream
      case result of
        Left () -> pure ()
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

--------------------------------------------------------------------------------
-- Set and delete: document rewriting
--------------------------------------------------------------------------------

-- | What happens to a value focused by an optic: it is replaced with a
-- sequence of events (@set@), removed entirely (@delete@), or rewritten
-- by a transformation (@over@).
data Splicer
  = SpliceSet [JSONEvent]
  | SpliceDelete
  | SpliceTransform Transformation
  deriving (Show, Eq)

-- | Replace every value the optic focuses on with the given replacement
-- events, passing the rest of the document through unchanged (lens
-- @set@).  The replacement is spliced into the output verbatim.
runSet :: Optic -> [JSONEvent] -> Cursor -> ExceptT Text IO ValueStream
runSet optic replacement = runSplice (SpliceSet replacement) optic

-- | Remove every value the optic focuses on from the document, passing
-- the rest of the document through unchanged (lens @delete@/@omit@).
runDelete :: Optic -> Cursor -> ExceptT Text IO ValueStream
runDelete = runSplice SpliceDelete

-- | Apply a transformation to every value the optic focuses on, rewriting
-- the document in place (lens @over@).
runOver :: Optic -> Transformation -> Cursor -> ExceptT Text IO ValueStream
runOver optic transformation = runSplice (SpliceTransform transformation) optic

-- | Rewrite the document at the cursor by splicing every value that the
-- optic focuses on.
--
-- This is the streaming counterpart to 'runOptic': instead of
-- extracting the focused values, the @rewrite@ family re-emits the
-- document, replacing or omitting the focused values in place.  All
-- other events pass through unchanged, so the output is the input with
-- only the targeted values modified.
runSplice :: Splicer -> Optic -> Cursor -> ExceptT Text IO ValueStream
runSplice splicer (Optic optic) = run optic
  where
    -- \| Apply the whole optic at a value.
    run :: Fix OpticF -> Cursor -> ExceptT Text IO ValueStream
    run step cursor = case unFix step of
      Id -> spliceValue splicer cursor
      _ -> navigate step (Fix Id) cursor

    -- \| Navigate one optic step, splicing the remainder at each landing
    -- and passing everything else through.
    navigate :: Fix OpticF -> Fix OpticF -> Cursor -> ExceptT Text IO ValueStream
    navigate step suffix cursor = case unFix step of
      Id -> run suffix cursor
      Compose left right -> navigate left (composeStep right suffix) cursor
      Field name -> rewriteField name cursor suffix
      Each -> rewriteEach cursor suffix
      PrismString -> rewritePrism isString cursor suffix
      PrismNumber -> rewritePrism isNumber cursor suffix
      PrismBool -> rewritePrism isBool cursor suffix
      PrismNull -> rewritePrism isNull cursor suffix
      PrismArray -> rewritePrism isArray cursor suffix
      PrismObject -> rewritePrism isObject cursor suffix
      PrismJust -> rewriteJust cursor suffix
      Prism1 -> rewriteIndex 0 cursor suffix
      Prism2 -> rewriteIndex 1 cursor suffix
      Ix i -> rewriteIndex i cursor suffix

    -- \| Compose two optic steps, normalizing away the identity optic.
    --
    -- Without this, reassociating a composition such as
    -- @Compose a (Compose b Id)@ repeatedly rebuilds @Compose Id Id@
    -- spines that the navigator would keep re-arranging forever.
    composeStep :: Fix OpticF -> Fix OpticF -> Fix OpticF
    composeStep (Fix Id) rest = rest
    composeStep step rest = Fix (Compose step rest)

    -- \| Emit the replacement for a value the whole optic lands on.
    spliceValue :: Splicer -> Cursor -> ExceptT Text IO ValueStream
    spliceValue (SpliceSet events) _ = pure (S.each events)
    spliceValue SpliceDelete _ = pure mempty
    spliceValue (SpliceTransform transformation) cursor = do
      mConsumed <- consumeValue cursor
      case mConsumed of
        Nothing -> throwError "unexpected end of input"
        Just (events, _) ->
          case eventsToValue events >>= runTransformation transformation of
            Left err -> throwError err
            Right value -> pure (S.each (valueToEvents value))

    -- \| Does the part of the optic that remains after @step@ land on
    -- the value at the cursor itself, rather than navigating into one
    -- of its children?
    --
    -- Used by @delete@ to decide whether an object member loses both
    -- its key and its value (a landing) or keeps its key while its
    -- value is rewritten (e.g. @delete #users.each.#name@).
    landing :: Fix OpticF -> Cursor -> ExceptT Text IO Bool
    landing f cursor = case unFix f of
      Id -> pure True
      Field _ -> pure False
      Each -> pure False
      Ix _ -> pure False
      Prism1 -> pure False
      Prism2 -> pure False
      PrismString -> peek isString
      PrismNumber -> peek isNumber
      PrismBool -> peek isBool
      PrismNull -> peek isNull
      PrismArray -> peek isArray
      PrismObject -> peek isObject
      PrismJust -> peek (not . isNull)
      Compose l r -> do
        lLands <- landing l cursor
        if lLands then landing r cursor else pure False
      where
        peek predicate = do
          result <- next cursor
          pure $ case result of
            Nothing -> False
            Just (event, _) -> predicate event

    -- \| A value whose first event satisfies the predicate is handed to
    -- the remainder of the optic; anything else passes through.
    rewritePrism :: (JSONEvent -> Bool) -> Cursor -> Fix OpticF -> ExceptT Text IO ValueStream
    rewritePrism predicate cursor suffix =
      next cursor >>= \case
        Nothing -> pure mempty
        Just (event, rest)
          | predicate event -> run suffix (fromFirst event rest)
          | otherwise -> emitValue (fromFirst event rest)

    -- \| Prism on any non-null value.
    rewriteJust :: Cursor -> Fix OpticF -> ExceptT Text IO ValueStream
    rewriteJust cursor suffix =
      next cursor >>= \case
        Nothing -> pure mempty
        Just (JSONNull, _) -> pure (S.yield JSONNull)
        Just (event, rest) -> run suffix (fromFirst event rest)

    -- \| Descend into an array, splicing the remainder of the optic at
    -- the element at @index@.
    rewriteIndex :: Int -> Cursor -> Fix OpticF -> ExceptT Text IO ValueStream
    rewriteIndex index cursor suffix =
      next cursor >>= \case
        Nothing -> pure mempty
        Just (event, rest) -> case event of
          JSONBeginArray -> pure (rewriteIndexElements index rest suffix)
          _ -> emitValue (fromFirst event rest)

    -- \| Emit a whole array, splicing the remainder of the optic at the
    -- element at index @target@ and passing every other element
    -- through.
    rewriteIndexElements :: Int -> Cursor -> Fix OpticF -> ValueStream
    rewriteIndexElements target c suffix = do
      S.yield JSONBeginArray
      go target c
      where
        go n arr = do
          mNext <- lift $ next arr
          case mNext of
            Nothing -> lift $ throwError "unexpected end of input while reading array"
            Just (event, rest) -> case event of
              JSONEndArray -> S.yield JSONEndArray
              _ ->
                let element = fromFirst event rest
                 in consumeElement element $ \originalEvents afterValue ->
                      if n == 0
                        then do
                          join (lift $ run suffix (mkCursorFromList originalEvents))
                          go (n - 1) afterValue
                        else do
                          S.each originalEvents
                          go (n - 1) afterValue

    -- \| Descend into an object, splicing the value of the member named
    -- @name@.
    rewriteField :: Text -> Cursor -> Fix OpticF -> ExceptT Text IO ValueStream
    rewriteField name cursor suffix =
      next cursor >>= \case
        Nothing -> pure mempty
        Just (event, rest) -> case event of
          JSONBeginObject -> pure (rewriteObjectPairs name rest suffix)
          _ -> emitValue (fromFirst event rest)

    -- \| Emit a whole object, splicing the member named @name@ and
    -- passing every other member through.
    rewriteObjectPairs :: Text -> Cursor -> Fix OpticF -> ValueStream
    rewriteObjectPairs name c suffix = do
      S.yield JSONBeginObject
      go c
      where
        go obj = do
          mNext <- lift $ next obj
          case mNext of
            Nothing -> lift $ throwError "unexpected end of input while reading object"
            Just (event, rest) -> case event of
              JSONEndObject -> S.yield JSONEndObject
              JSONObjectKey key -> do
                mVal <- lift $ next rest
                case mVal of
                  Nothing -> lift $ throwError "unexpected end of input after object key"
                  Just (valueEvent, valueRest) -> do
                    let value = fromFirst valueEvent valueRest
                    consumeElement value $ \originalEvents afterValue ->
                      if key == name
                        then spliceMember (JSONObjectKey key) (mkCursorFromList originalEvents) afterValue suffix go
                        else do
                          S.yield (JSONObjectKey key)
                          S.each originalEvents
                          go afterValue
              _ -> lift $ throwError "invalid JSON object"

    -- \| Traverse every element of an array (the @each@ traversal).
    rewriteEach :: Cursor -> Fix OpticF -> ExceptT Text IO ValueStream
    rewriteEach cursor suffix =
      next cursor >>= \case
        Nothing -> pure mempty
        Just (event, rest) -> case event of
          JSONBeginArray -> pure (rewriteAllElements rest suffix)
          JSONBeginObject -> pure (rewriteAllMembers rest suffix)
          _ -> pure (S.yield event)

    -- \| Emit a whole array, splicing the remainder of the optic at
    -- every element.
    rewriteAllElements :: Cursor -> Fix OpticF -> ValueStream
    rewriteAllElements c suffix = do
      S.yield JSONBeginArray
      go c
      where
        go arr = do
          mNext <- lift $ next arr
          case mNext of
            Nothing -> lift $ throwError "unexpected end of input while reading array"
            Just (event, rest) -> case event of
              JSONEndArray -> S.yield JSONEndArray
              _ ->
                let element = fromFirst event rest
                 in consumeElement element $ \originalEvents afterValue -> do
                      join (lift $ run suffix (mkCursorFromList originalEvents))
                      go afterValue

    -- \| Emit a whole object, splicing the remainder of the optic at
    -- every member value.
    rewriteAllMembers :: Cursor -> Fix OpticF -> ValueStream
    rewriteAllMembers c suffix = do
      S.yield JSONBeginObject
      go c
      where
        go obj = do
          mNext <- lift $ next obj
          case mNext of
            Nothing -> lift $ throwError "unexpected end of input while reading object"
            Just (event, rest) -> case event of
              JSONEndObject -> S.yield JSONEndObject
              JSONObjectKey key -> do
                mVal <- lift $ next rest
                case mVal of
                  Nothing -> lift $ throwError "unexpected end of input after object key"
                  Just (valueEvent, valueRest) -> do
                    let value = fromFirst valueEvent valueRest
                    consumeElement value $ \originalEvents afterValue ->
                      spliceMember (JSONObjectKey key) (mkCursorFromList originalEvents) afterValue suffix go
              _ -> lift $ throwError "invalid JSON object"

    -- \| Splice one object member.  The key survives unless a @delete@
    -- lands on the member value as a whole; the walk then continues
    -- with @continue@.
    spliceMember :: JSONEvent -> Cursor -> Cursor -> Fix OpticF -> (Cursor -> ValueStream) -> ValueStream
    spliceMember key freshCursor afterValue suffix continue =
      case splicer of
        SpliceDelete -> do
          lands <- lift $ landing suffix freshCursor
          if lands
            then continue afterValue
            else do
              S.yield key
              join (lift $ run suffix freshCursor)
              continue afterValue
        SpliceSet _ -> do
          S.yield key
          join (lift $ run suffix freshCursor)
          continue afterValue
        SpliceTransform _ -> do
          S.yield key
          join (lift $ run suffix freshCursor)
          continue afterValue

    -- \| Consume a complete value, handing its materialized events and
    -- the cursor positioned just after it to the continuation.
    consumeElement :: Cursor -> ([JSONEvent] -> Cursor -> ValueStream) -> ValueStream
    consumeElement cursor k = do
      mConsumed <- lift $ consumeValue cursor
      case mConsumed of
        Nothing -> lift $ throwError "unexpected end of input"
        Just (events, afterValue) -> k events afterValue

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
  hSetBuffering stdout (BlockBuffering Nothing)
  r <- runExceptT $ do
    streamIO <- runRunner runner query handle
    S.mapM_ write $ encode encConfig 32 streamIO
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
executeQuery (Preview optic) val = runPreview optic val
executeQuery (Fold optic) val = runFold optic val
executeQuery (Set optic value) val = runSet optic value val
executeQuery (Over optic transformation) val = runOver optic transformation val
executeQuery (Delete optic) val = runDelete optic val

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
