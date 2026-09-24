{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner.Splice where

import Control.Monad.Error.Class (MonadError (throwError))
import Data.Aeson (Value)
import Data.Fix (Fix (..))
import Data.Functor.Of (Of ((:>)))
import HQ.JSON.Encoder (ChunkStream, EncodeCtx, EncoderConfig)
import HQ.JSON.Event
import HQ.Optic (Optic (..), OpticF (..))
import HQ.Runner.Cursor (Cursor (..), KSplice, pullCursor, pushCursor, skipValueE)
import HQ.Runner.Take (emitChunk, takeValue, takeValueChunks)
import HQ.Transformation (Transformation (..), TransformationF (..), runTransformation)
import Relude hiding (Compose, Const)
import qualified Streaming.Prelude as S

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
