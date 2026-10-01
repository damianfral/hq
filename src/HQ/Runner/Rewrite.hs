{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner.Rewrite where

import Control.Monad.Except (MonadError (..))
import Data.Aeson (Value (..))
import Data.Fix (Fix (..))
import HQ.Error (HQError (..))
import HQ.JSON.Encoder (ChunkStream, EncoderConfig, EncoderState)
import HQ.JSON.Event
import HQ.Optic (Optic (..), OpticF (..), appendOptic, focusesWhole, prismPredicate)
import HQ.Runner.Cursor (Cursor (..), RewriteContinuation, expectArrayStep, pullCursor, pushCursor, skipValueE)
import HQ.Runner.Error (RunnerError (..))
import HQ.Runner.Fold (applyTransformation, gateValue, materializeValue)
import HQ.Runner.Take (emitChunk, emitKeyAndTake, onEventOrEndChunks, takeValueChunks, traverseArrayChunks, traverseObjectChunks)
import HQ.Transformation (Transformation (..), TransformationF (Const), runTransformation)
import HQ.Transformation.Error (TransformationError (..))
import Relude hiding (Compose, Const)

--------------------------------------------------------------------------------
-- Over and delete: document rewriting
--------------------------------------------------------------------------------

-- | What happens to a focused value: removed (@delete@) or rewritten
-- (@over@, including @set@ as @over@ with a constant).
data Rewriter = RewriteDelete | RewriteTransform Transformation
  deriving (Show, Eq)

data KeyAction = DropKey | KeepKey | RenameKey Text
  deriving (Show, Eq)

runDelete :: Optic -> EncoderConfig -> RewriteContinuation
runDelete = runRewrite RewriteDelete

runOver :: Optic -> Transformation -> EncoderConfig -> RewriteContinuation
runOver optic transformation = runRewrite (RewriteTransform transformation) optic

-- | Rewrite the document at the cursor, replacing or omitting focused
-- values in place; everything else passes through unchanged.
runRewrite :: Rewriter -> Optic -> EncoderConfig -> RewriteContinuation
runRewrite rewriter (Optic optic) config = run optic
  where
    run :: Fix OpticF -> RewriteContinuation
    run step input st = case unFix step of
      Id -> rewriteValue rewriter input st
      _ -> navigate step (Fix Id) input st

    navigate :: Fix OpticF -> Fix OpticF -> RewriteContinuation
    navigate step suffix input st = case unFix step of
      Id -> run suffix input st
      Compose left right -> navigate left (appendOptic right suffix) input st
      Field name -> rewriteField name suffix input st
      Each -> rewriteEach suffix input st
      Keys -> rewriteKeys suffix input st
      Values -> rewriteValues suffix input st
      Prism kind -> rewritePrism (prismPredicate kind) suffix input st
      PrismJust -> rewriteJust suffix input st
      Ix i -> rewriteIndex i suffix input st
      Filter o t -> rewriteFilter o t suffix input st

    -- \| Test a @filter@ gate, returning whether it is kept plus
    -- replay/after cursors.
    gateTake ::
      Fix OpticF ->
      Transformation ->
      Cursor ->
      ExceptT HQError IO (Bool, JSONEvent, Cursor, Cursor)
    gateTake o t cur = do
      (v, events, afterValue) <- materializeValue cur
      keep <- gateValue o t v
      case events of
        [] -> throwError $ HQRunnerError EmptyValue
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

    rewriteValue :: Rewriter -> RewriteContinuation
    rewriteValue RewriteDelete input st = do
      after <- lift (skipValueE input)
      pure (st, after)
    -- A constant replacement never reads the focused value: skip it
    -- at the text level instead of decoding it.
    rewriteValue (RewriteTransform (Transformation (Fix (Const value)))) input st = do
      after <- lift (skipValueE input)
      emitValueChunks value st after
    rewriteValue (RewriteTransform t) input st = do
      (v, _, rest) <- lift (materializeValue input)
      value <- lift (applyTransformation t v)
      emitValueChunks value st rest

    -- \| Emit a transformed value's events as chunks.
    emitValueChunks :: Value -> EncoderState -> Cursor -> ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
    emitValueChunks value st cursor = go st (valueToEvents value)
      where
        go s [] = pure (s, cursor)
        go s (event : events) = do
          s' <- emitChunk config event s
          go s' events

    -- \| Prism: value whose first event satisfies the predicate goes
    -- through the suffix; everything else passes through unchanged.
    rewritePrism :: (JSONEvent -> Bool) -> Fix OpticF -> RewriteContinuation
    rewritePrism predicate suffix input st =
      onEventOrEndChunks input st $ \(event, rest) ->
        if predicate event
          then run suffix (pushCursor event rest) st
          else takeValueChunks config (pushCursor event rest) st

    -- \| Prism on non-null: null passes through, anything else goes
    -- through the suffix.
    rewriteJust :: Fix OpticF -> RewriteContinuation
    rewriteJust suffix input st =
      onEventOrEndChunks input st $ \case
        (JSONNull, rest) -> do
          st' <- emitChunk config JSONNull st
          pure (st', rest)
        (event, rest) -> run suffix (pushCursor event rest) st

    -- \| Array index: rewrite the element at @index@, pass through the
    -- rest of the array. Non-arrays pass through unchanged.
    rewriteIndex :: Int -> Fix OpticF -> RewriteContinuation
    rewriteIndex index suffix input st =
      onEventOrEndChunks input st $ \case
        (JSONBeginArray, rest) -> do
          st' <- emitChunk config JSONBeginArray st
          go index rest st'
        (event, rest) -> takeValueChunks config (pushCursor event rest) st
      where
        go n stream s = do
          step <- lift (expectArrayStep stream)
          case step of
            Left rest -> do
              s' <- emitChunk config JSONEndArray s
              pure (s', rest)
            Right (event, rest)
              | n == 0 -> do
                  (s', after) <- run suffix (pushCursor event rest) s
                  go (-1) after s'
              | otherwise -> do
                  (s', after) <- takeValueChunks config (pushCursor event rest) s
                  go (n - 1) after s'

    -- \| Object field: rewrite the member named @name@ (dropping the key
    -- too under @delete@ when the suffix lands on the value), pass
    -- every other member through unchanged.
    rewriteField :: Text -> Fix OpticF -> RewriteContinuation
    rewriteField name suffix input st =
      onEventOrEndChunks input st $ \case
        (JSONBeginObject, rest) -> do
          st' <- emitChunk config JSONBeginObject st
          traverseObjectChunks config rest st' $ \key rest' s ->
            if key == name
              then rewriteOneMember suffix key rest' s
              else emitKeyAndTake config key rest' s
        (event, rest) -> takeValueChunks config (pushCursor event rest) st

    -- \| The @each@ traversal: rewrite every array element, or every
    -- object member value. Non-containers pass through.
    rewriteEach :: Fix OpticF -> RewriteContinuation
    rewriteEach suffix input st =
      onEventOrEndChunks input st $ \case
        (JSONBeginArray, rest) -> do
          st' <- emitChunk config JSONBeginArray st
          traverseArrayChunks config rest st' $ \(event, rest') s ->
            run suffix (pushCursor event rest') s
        (JSONBeginObject, rest) -> do
          st' <- emitChunk config JSONBeginObject st
          traverseObjectChunks config rest st' $ \key rest' s ->
            rewriteOneMember suffix key rest' s
        (event, rest) -> takeValueChunks config (pushCursor event rest) st

    -- \| The @values@ traversal: rewrite every object member value.
    -- Arrays and scalars pass through unchanged (unlike 'rewriteEach').
    rewriteValues :: Fix OpticF -> RewriteContinuation
    rewriteValues suffix input st =
      onEventOrEndChunks input st $ \case
        (JSONBeginObject, rest) -> do
          st' <- emitChunk config JSONBeginObject st
          traverseObjectChunks config rest st' $ \key rest' s ->
            rewriteOneMember suffix key rest' s
        (event, rest) -> takeValueChunks config (pushCursor event rest) st

    -- \| The @filter@ optic: gate the focused value as a whole, running
    -- the suffix on kept values and passing dropped values through
    -- unchanged.
    rewriteFilter :: Fix OpticF -> Transformation -> Fix OpticF -> RewriteContinuation
    rewriteFilter o t suffix input st =
      onEventOrEndChunks input st $ \(event, rest) -> do
        (keep, _, valCursor, _) <- lift (gateTake o t (pushCursor event rest))
        if keep
          then run suffix valCursor st
          else takeValueChunks config valCursor st

    -- \| The @keys@ traversal: rewrite object keys, leaving values alone.
    -- Arrays and scalars pass through unchanged (@keys@ is objects-only).
    rewriteKeys :: Fix OpticF -> RewriteContinuation
    rewriteKeys suffix input st =
      onEventOrEndChunks input st $ \case
        (JSONBeginObject, rest) -> do
          st' <- emitChunk config JSONBeginObject st
          traverseObjectChunks config rest st' $ \key rest' s -> do
            action <- lift (rewriteKeyText suffix rewriter key)
            applyKeyAction action key rest' s
        (event, rest) -> takeValueChunks config (pushCursor event rest) st
      where
        applyKeyAction DropKey _ rest s = do
          after <- lift (skipValueE rest)
          pure (s, after)
        applyKeyAction KeepKey key rest s = emitKeyAndTake config key rest s
        applyKeyAction (RenameKey newKey) _ rest s = emitKeyAndTake config newKey rest s

    -- \| Apply the remainder of a @keys@ optic plus the enclosing
    -- 'Rewriter' to one object key. Keys are scalar strings, so the
    -- suffix either focuses the key as a whole (checked with
    -- 'focusesWhole', e.g. @Id@ or @_String@) or it matches nothing and
    -- the key is kept.
    rewriteKeyText :: Fix OpticF -> Rewriter -> Text -> ExceptT HQError IO KeyAction
    rewriteKeyText suffix rw key = case rw of
      RewriteDelete
        | focusesWhole suffix (JSONString key) -> pure DropKey
        | otherwise -> pure KeepKey
      RewriteTransform t
        | focusesWhole suffix (JSONString key) -> case runTransformation t (String key) of
            Left err -> throwError $ HQTransformationError err
            Right (String next) -> pure (if next == key then KeepKey else RenameKey next)
            Right _ -> throwError $ HQTransformationError KeyNotString
        | otherwise -> pure KeepKey

    -- \| Rewrite one object member, returning the cursor after it.
    -- The key survives unless a @delete@ lands on the member value as
    -- a whole. A leading @filter@ step gates the value first: dropped
    -- members pass through, and kept members deleted as a whole lose
    -- their key too. The object walk itself is shared via
    -- 'traverseObjectChunks'.
    rewriteOneMember :: Fix OpticF -> Text -> Cursor -> EncoderState -> ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
    rewriteOneMember suffix key stream st = do
      result <- lift (pullCursor stream)
      case result of
        Nothing -> throwError $ HQRunnerError ExpectedMemberValue
        Just (event, rest) -> case stripFilterGate suffix of
          Just (o, t, rest') -> rewriteFilteredMember o t rest' key event rest st
          Nothing -> rewritePlainMember suffix key event rest st

    rewriteFilteredMember :: Fix OpticF -> Transformation -> Fix OpticF -> Text -> JSONEvent -> Cursor -> EncoderState -> ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
    rewriteFilteredMember o t rest' key event rest st = do
      (keep, firstEv, valCursor, afterValue) <- lift (gateTake o t (pushCursor event rest))
      if not keep
        then emitKeyAndTake config key valCursor st
        else case (rewriter, focusesWhole rest' firstEv) of
          (RewriteDelete, True) -> pure (st, afterValue)
          _ -> do
            st' <- emitChunk config (JSONObjectKey key) st
            run rest' valCursor st'

    rewritePlainMember :: Fix OpticF -> Text -> JSONEvent -> Cursor -> EncoderState -> ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
    rewritePlainMember suffix key event rest st = case rewriter of
      RewriteDelete
        | focusesWhole suffix event -> do
            after <- lift (skipValueE (pushCursor event rest))
            pure (st, after)
        | otherwise -> do
            st' <- emitChunk config (JSONObjectKey key) st
            run suffix (pushCursor event rest) st'
      RewriteTransform _ -> do
        st' <- emitChunk config (JSONObjectKey key) st
        run suffix (pushCursor event rest) st'
