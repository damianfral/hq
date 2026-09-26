{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner.Rewrite where

import Control.Monad.Error.Class (MonadError (throwError))
import Data.Aeson (Value (..))
import Data.Fix (Fix (..))
import Data.Functor.Of (Of ((:>)))
import HQ.JSON.Encoder (ChunkStream, EncodeCtx, EncoderConfig)
import HQ.JSON.Event
import HQ.Optic (Optic (..), OpticF (..), prismPredicate)
import HQ.Runner.Cursor (Cursor (..), KRewrite, pullCursor, pushCursor, skipValueE)
import HQ.Runner.Fold (evalFilterGate)
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
data Rewriter = RewriteDelete | RewriteTransform Transformation
  deriving (Show, Eq)

-- | What rewriting through @keys@ does to a single object key.
data KeyAction = DropKey | KeepKey | RenameKey Text
  deriving (Show, Eq)

-- | Remove every value the optic focuses on from the document, passing
-- the rest of the document through unchanged (lens @delete@/@omit@).
runDelete :: Optic -> EncoderConfig -> KRewrite
runDelete = runRewrite RewriteDelete

-- | Apply a transformation to every value the optic focuses on, rewriting
-- the document in place (lens @over@).
runOver :: Optic -> Transformation -> EncoderConfig -> KRewrite
runOver optic transformation = runRewrite (RewriteTransform transformation) optic

-- | Rewrite the document at the cursor by rewriting every value that the
-- optic focuses on.
--
-- This is the streaming counterpart to 'runOptic': instead of
-- extracting the focused values, the @rewrite@ family re-emits the
-- document, replacing or omitting the focused values in place.  All
-- other events pass through unchanged, so the output is the input with
-- only the targeted values modified.
runRewrite :: Rewriter -> Optic -> EncoderConfig -> KRewrite
runRewrite rewriter (Optic optic) config = run optic
  where
    run :: Fix OpticF -> KRewrite
    run step input ctxs = case unFix step of
      Id -> rewriteValue rewriter input ctxs
      _ -> navigate step (Fix Id) input ctxs

    navigate :: Fix OpticF -> Fix OpticF -> KRewrite
    navigate step suffix input ctxs = case unFix step of
      Id -> run suffix input ctxs
      Compose left right -> navigate left (composeStep right suffix) input ctxs
      Field name -> rewriteField name suffix input ctxs
      Each -> rewriteEach suffix input ctxs
      Keys -> rewriteKeys suffix input ctxs
      Values -> rewriteValues suffix input ctxs
      Prism kind -> rewritePrism (prismPredicate kind) suffix input ctxs
      PrismJust -> rewriteJust suffix input ctxs
      Ix i -> rewriteIndex i suffix input ctxs
      Filter o t -> rewriteFilter o t suffix input ctxs

    composeStep :: Fix OpticF -> Fix OpticF -> Fix OpticF
    composeStep (Fix Id) rest = rest
    composeStep step rest = Fix (Compose step rest)

    -- \| Materialize one value and test a @filter@ gate, returning
    -- whether it is kept, its first event, a cursor replaying it, and
    -- the cursor after it.
    gateTake ::
      Fix OpticF ->
      Transformation ->
      Cursor ->
      ExceptT Text IO (Bool, JSONEvent, Cursor, Cursor)
    gateTake o t cur = do
      (events :> afterValue) <- S.toList (takeValue cur)
      v <- hoistEither $ eventsToValue events
      keep <- hoistEither $ evalFilterGate o t v
      case events of
        [] -> throwError "unexpected empty value"
        (firstEv : _) ->
          let Cursor _ dec txt = afterValue
           in pure (keep, firstEv, Cursor events dec txt, afterValue)

    -- \| Split a member-value suffix with a leading @filter@ step into
    -- its gate and remainder, so a kept value deleted as a whole drops
    -- its key too.
    stripFilterGate :: Fix OpticF -> Maybe (Fix OpticF, Transformation, Fix OpticF)
    stripFilterGate (Fix (Compose (Fix (Filter o t)) rest')) = Just (o, t, rest')
    stripFilterGate (Fix (Filter o t)) = Just (o, t, Fix Id)
    stripFilterGate _ = Nothing

    rewriteValue :: Rewriter -> KRewrite
    rewriteValue RewriteDelete input ctxs = do
      after <- lift (skipValueE input)
      pure (ctxs, after)
    -- A constant replacement never reads the focused value: skip it
    -- at the text level instead of decoding it.
    rewriteValue (RewriteTransform (Transformation (Fix (Const value)))) input ctxs = do
      after <- lift (skipValueE input)
      emitValueChunks value ctxs after
    rewriteValue (RewriteTransform t) input ctxs = do
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
      Keys -> False
      Values -> False
      Ix _ -> False
      Prism kind -> prismPredicate kind event
      PrismJust -> not (isNull event)
      Filter _ _ -> False
      Compose l r -> landing l event && landing r event

    -- \| Prism: value whose first event satisfies the predicate goes
    -- through the suffix; everything else passes through unchanged.
    rewritePrism :: (JSONEvent -> Bool) -> Fix OpticF -> KRewrite
    rewritePrism predicate suffix input ctxs = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure (ctxs, input)
        Just (event, rest)
          | predicate event -> run suffix (pushCursor event rest) ctxs
          | otherwise -> takeValueChunks config (pushCursor event rest) ctxs

    -- \| Prism on non-null: null passes through, anything else goes
    -- through the suffix.
    rewriteJust :: Fix OpticF -> KRewrite
    rewriteJust suffix input ctxs = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure (ctxs, input)
        Just (JSONNull, rest) -> do
          ctxs' <- emitChunk config JSONNull ctxs
          pure (ctxs', rest)
        Just (event, rest) -> run suffix (pushCursor event rest) ctxs

    -- \| Array index: rewrite the element at @index@, pass through the
    -- rest of the array. Non-arrays pass through unchanged.
    rewriteIndex :: Int -> Fix OpticF -> KRewrite
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

    -- \| Object field: rewrite the member named @name@ (dropping the key
    -- too under @delete@ when the suffix lands on the value), pass
    -- every other member through unchanged.
    rewriteField :: Text -> Fix OpticF -> KRewrite
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
              | key == name -> rewriteMember suffix key rest pairs ctx
              | otherwise -> do
                  ctx' <- emitChunk config (JSONObjectKey key) ctx
                  (ctx'', after) <- takeValueChunks config rest ctx'
                  pairs after ctx''
            Just _ -> throwError "invalid JSON object"

    -- \| The @each@ traversal: rewrite every array element, or every
    -- object member value. Non-containers pass through.
    rewriteEach :: Fix OpticF -> KRewrite
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
              rewriteMember suffix key rest allMembers ctx
            Just _ -> throwError "invalid JSON object"

    -- \| The @values@ traversal: rewrite every object member value.
    -- Arrays and scalars pass through unchanged (unlike 'rewriteEach').
    rewriteValues :: Fix OpticF -> KRewrite
    rewriteValues suffix input ctxs = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure (ctxs, input)
        Just (JSONBeginObject, rest) -> do
          ctxs' <- emitChunk config JSONBeginObject ctxs
          allMembers rest ctxs'
        Just (event, rest) -> takeValueChunks config (pushCursor event rest) ctxs
      where
        allMembers :: Cursor -> [EncodeCtx] -> ChunkStream (ExceptT Text IO) ([EncodeCtx], Cursor)
        allMembers stream ctx = do
          result <- lift (pullCursor stream)
          case result of
            Nothing -> throwError "unexpected end of input while reading object"
            Just (JSONEndObject, rest) -> do
              ctx' <- emitChunk config JSONEndObject ctx
              pure (ctx', rest)
            Just (JSONObjectKey key, rest) ->
              rewriteMember suffix key rest allMembers ctx
            Just _ -> throwError "invalid JSON object"

    -- \| The @filter@ optic: gate the focused value as a whole, running
    -- the suffix on kept values and passing dropped values through
    -- unchanged.
    rewriteFilter :: Fix OpticF -> Transformation -> Fix OpticF -> KRewrite
    rewriteFilter o t suffix input ctxs = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure (ctxs, input)
        Just (event, rest) -> do
          (keep, _, valCursor, _) <- lift (gateTake o t (pushCursor event rest))
          if keep
            then run suffix valCursor ctxs
            else takeValueChunks config valCursor ctxs

    -- \| The @keys@ traversal: rewrite object keys, leaving values alone.
    -- Arrays and scalars pass through unchanged (@keys@ is objects-only).
    rewriteKeys :: Fix OpticF -> KRewrite
    rewriteKeys suffix input ctxs = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure (ctxs, input)
        Just (JSONBeginObject, rest) -> do
          ctxs' <- emitChunk config JSONBeginObject ctxs
          allKeys rest ctxs'
        Just (event, rest) -> takeValueChunks config (pushCursor event rest) ctxs
      where
        allKeys :: Cursor -> [EncodeCtx] -> ChunkStream (ExceptT Text IO) ([EncodeCtx], Cursor)
        allKeys stream ctx = do
          result <- lift (pullCursor stream)
          case result of
            Nothing -> throwError "unexpected end of input while reading object"
            Just (JSONEndObject, rest) -> do
              ctx' <- emitChunk config JSONEndObject ctx
              pure (ctx', rest)
            Just (JSONObjectKey key, rest) -> do
              action <- lift (rewriteKeyText suffix rewriter key)
              case action of
                DropKey -> do
                  after <- lift (skipValueE rest)
                  allKeys after ctx
                KeepKey -> do
                  ctx' <- emitChunk config (JSONObjectKey key) ctx
                  (ctx'', after) <- takeValueChunks config rest ctx'
                  allKeys after ctx''
                RenameKey newKey -> do
                  ctx' <- emitChunk config (JSONObjectKey newKey) ctx
                  (ctx'', after) <- takeValueChunks config rest ctx'
                  allKeys after ctx''
            Just _ -> throwError "invalid JSON object"

    -- \| Apply the remainder of a @keys@ optic plus the enclosing
    -- 'Rewriter' to one object key. Keys are scalar strings, so the
    -- suffix either focuses the key as a whole (checked with
    -- 'landing', e.g. @Id@ or @_String@) or it matches nothing and
    -- the key is kept.
    rewriteKeyText :: Fix OpticF -> Rewriter -> Text -> ExceptT Text IO KeyAction
    rewriteKeyText suffix rw key = case rw of
      RewriteDelete
        | landing suffix (JSONString key) -> pure DropKey
        | otherwise -> pure KeepKey
      RewriteTransform t
        | landing suffix (JSONString key) -> case runTransformation t (String key) of
            Left err -> throwError err
            Right (String next) -> pure (if next == key then KeepKey else RenameKey next)
            Right _ -> throwError "key transformation must yield a string"
        | otherwise -> pure KeepKey

    -- \| Rewrite one object member. The key survives unless a @delete@
    -- lands on the member value as a whole; the walk then continues
    -- with @continue@. A leading @filter@ step gates the value first:
    -- dropped members pass through, and kept members deleted as a whole
    -- lose their key too.
    rewriteMember :: Fix OpticF -> Text -> Cursor -> KRewrite -> [EncodeCtx] -> ChunkStream (ExceptT Text IO) ([EncodeCtx], Cursor)
    rewriteMember suffix key stream continue ctxs = do
      result <- lift (pullCursor stream)
      case result of
        Nothing -> throwError "unexpected end of input after object key"
        Just (event, rest) -> case stripFilterGate suffix of
          Just (o, t, rest') -> do
            (keep, firstEv, valCursor, afterValue) <- lift (gateTake o t (pushCursor event rest))
            if not keep
              then do
                ctx' <- emitChunk config (JSONObjectKey key) ctxs
                (ctx'', after) <- takeValueChunks config valCursor ctx'
                continue after ctx''
              else case (rewriter, landing rest' firstEv) of
                (RewriteDelete, True) -> continue afterValue ctxs
                _ -> do
                  ctx' <- emitChunk config (JSONObjectKey key) ctxs
                  (ctx'', after) <- run rest' valCursor ctx'
                  continue after ctx''
          Nothing -> case rewriter of
            RewriteDelete
              | landing suffix event -> do
                  after <- lift (skipValueE (pushCursor event rest))
                  continue after ctxs
              | otherwise -> do
                  ctx' <- emitChunk config (JSONObjectKey key) ctxs
                  (ctx'', after) <- run suffix (pushCursor event rest) ctx'
                  continue after ctx''
            RewriteTransform _ -> do
              ctx' <- emitChunk config (JSONObjectKey key) ctxs
              (ctx'', after) <- run suffix (pushCursor event rest) ctx'
              continue after ctx''
