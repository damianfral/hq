{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner.Rewrite where

import Control.Monad.Except (MonadError (..))
import Data.Aeson (Value (..))
import Data.Fix (Fix (..))
import HQ.Error (HQError (..))
import HQ.JSON.Encoder (ChunkStream, EncoderConfig, EncoderState)
import HQ.JSON.Event
import HQ.Optic (Optic (..), OpticF (..), appendOptic, focusesWhole, prismPredicate)
import HQ.Runner.Cursor (Cursor (..), RewriteContinuation, expectArrayStep, expectObjectStep, pullCursor, pushCursor, skipValueE)
import HQ.Runner.Error (RunnerError (..))
import HQ.Runner.Fold (applyTransformation, gateValue, materializeValue)
import HQ.Runner.Take (emitChunk, takeValueChunks)
import HQ.Transformation (Transformation (..), TransformationF (..), runTransformation)
import HQ.Transformation.Error (TransformationError (..))
import Relude hiding (Compose, Const)

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
runDelete :: Optic -> EncoderConfig -> RewriteContinuation
runDelete = runRewrite RewriteDelete

-- | Apply a transformation to every value the optic focuses on, rewriting
-- the document in place (lens @over@).
runOver :: Optic -> Transformation -> EncoderConfig -> RewriteContinuation
runOver optic transformation = runRewrite (RewriteTransform transformation) optic

-- | Rewrite the document at the cursor by rewriting every value that the
-- optic focuses on.
--
-- This is the streaming counterpart to 'runOptic': instead of
-- extracting the focused values, the @rewrite@ family re-emits the
-- document, replacing or omitting the focused values in place.  All
-- other events pass through unchanged, so the output is the input with
-- only the targeted values modified.
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

    -- \| Materialize one value and test a @filter@ gate, returning
    -- whether it is kept, its first event, a cursor replaying it, and
    -- the cursor after it. Materialization and gating are shared with
    -- 'HQ.Runner.Fold' ('materializeValue', 'gateValue').
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
    rewritePrism predicate suffix input st = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure (st, input)
        Just (event, rest)
          | predicate event -> run suffix (pushCursor event rest) st
          | otherwise -> takeValueChunks config (pushCursor event rest) st

    -- \| Prism on non-null: null passes through, anything else goes
    -- through the suffix.
    rewriteJust :: Fix OpticF -> RewriteContinuation
    rewriteJust suffix input st = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure (st, input)
        Just (JSONNull, rest) -> do
          st' <- emitChunk config JSONNull st
          pure (st', rest)
        Just (event, rest) -> run suffix (pushCursor event rest) st

    -- \| Array index: rewrite the element at @index@, pass through the
    -- rest of the array. Non-arrays pass through unchanged.
    rewriteIndex :: Int -> Fix OpticF -> RewriteContinuation
    rewriteIndex index suffix input st = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure (st, input)
        Just (JSONBeginArray, rest) -> do
          st' <- emitChunk config JSONBeginArray st
          go index rest st'
        Just (event, rest) -> takeValueChunks config (pushCursor event rest) st
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
    rewriteField name suffix input st = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure (st, input)
        Just (JSONBeginObject, rest) -> do
          st' <- emitChunk config JSONBeginObject st
          pairs rest st'
        Just (event, rest) -> takeValueChunks config (pushCursor event rest) st
      where
        pairs :: Cursor -> EncoderState -> ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
        pairs stream s = do
          step <- lift (expectObjectStep stream)
          case step of
            Left rest -> do
              s' <- emitChunk config JSONEndObject s
              pure (s', rest)
            Right (key, rest)
              | key == name -> rewriteMember suffix key rest pairs s
              | otherwise -> do
                  s' <- emitChunk config (JSONObjectKey key) s
                  (s'', after) <- takeValueChunks config rest s'
                  pairs after s''

    -- \| The @each@ traversal: rewrite every array element, or every
    -- object member value. Non-containers pass through.
    rewriteEach :: Fix OpticF -> RewriteContinuation
    rewriteEach suffix input st = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure (st, input)
        Just (JSONBeginArray, rest) -> do
          st' <- emitChunk config JSONBeginArray st
          allElements rest st'
        Just (JSONBeginObject, rest) -> do
          st' <- emitChunk config JSONBeginObject st
          allMembers rest st'
        Just (event, rest) -> takeValueChunks config (pushCursor event rest) st
      where
        allElements :: Cursor -> EncoderState -> ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
        allElements stream s = do
          step <- lift (expectArrayStep stream)
          case step of
            Left rest -> do
              s' <- emitChunk config JSONEndArray s
              pure (s', rest)
            Right (event, rest) -> do
              (s', after) <- run suffix (pushCursor event rest) s
              allElements after s'

        allMembers :: Cursor -> EncoderState -> ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
        allMembers stream s = do
          step <- lift (expectObjectStep stream)
          case step of
            Left rest -> do
              s' <- emitChunk config JSONEndObject s
              pure (s', rest)
            Right (key, rest) ->
              rewriteMember suffix key rest allMembers s

    -- \| The @values@ traversal: rewrite every object member value.
    -- Arrays and scalars pass through unchanged (unlike 'rewriteEach').
    rewriteValues :: Fix OpticF -> RewriteContinuation
    rewriteValues suffix input st = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure (st, input)
        Just (JSONBeginObject, rest) -> do
          st' <- emitChunk config JSONBeginObject st
          allMembers rest st'
        Just (event, rest) -> takeValueChunks config (pushCursor event rest) st
      where
        allMembers :: Cursor -> EncoderState -> ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
        allMembers stream s = do
          step <- lift (expectObjectStep stream)
          case step of
            Left rest -> do
              s' <- emitChunk config JSONEndObject s
              pure (s', rest)
            Right (key, rest) ->
              rewriteMember suffix key rest allMembers s

    -- \| The @filter@ optic: gate the focused value as a whole, running
    -- the suffix on kept values and passing dropped values through
    -- unchanged.
    rewriteFilter :: Fix OpticF -> Transformation -> Fix OpticF -> RewriteContinuation
    rewriteFilter o t suffix input st = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure (st, input)
        Just (event, rest) -> do
          (keep, _, valCursor, _) <- lift (gateTake o t (pushCursor event rest))
          if keep
            then run suffix valCursor st
            else takeValueChunks config valCursor st

    -- \| The @keys@ traversal: rewrite object keys, leaving values alone.
    -- Arrays and scalars pass through unchanged (@keys@ is objects-only).
    rewriteKeys :: Fix OpticF -> RewriteContinuation
    rewriteKeys suffix input st = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure (st, input)
        Just (JSONBeginObject, rest) -> do
          st' <- emitChunk config JSONBeginObject st
          allKeys rest st'
        Just (event, rest) -> takeValueChunks config (pushCursor event rest) st
      where
        allKeys :: Cursor -> EncoderState -> ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
        allKeys stream s = do
          step <- lift (expectObjectStep stream)
          case step of
            Left rest -> do
              s' <- emitChunk config JSONEndObject s
              pure (s', rest)
            Right (key, rest) -> do
              action <- lift (rewriteKeyText suffix rewriter key)
              case action of
                DropKey -> do
                  after <- lift (skipValueE rest)
                  allKeys after s
                KeepKey -> do
                  s' <- emitChunk config (JSONObjectKey key) s
                  (s'', after) <- takeValueChunks config rest s'
                  allKeys after s''
                RenameKey newKey -> do
                  s' <- emitChunk config (JSONObjectKey newKey) s
                  (s'', after) <- takeValueChunks config rest s'
                  allKeys after s''

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

    -- \| Rewrite one object member. The key survives unless a @delete@
    -- lands on the member value as a whole; the walk then continues
    -- with @continue@. A leading @filter@ step gates the value first:
    -- dropped members pass through, and kept members deleted as a whole
    -- lose their key too.
    rewriteMember :: Fix OpticF -> Text -> Cursor -> RewriteContinuation -> EncoderState -> ChunkStream (ExceptT HQError IO) (EncoderState, Cursor)
    rewriteMember suffix key stream continue st = do
      result <- lift (pullCursor stream)
      case result of
        Nothing -> throwError $ HQRunnerError ExpectedMemberValue
        Just (event, rest) -> case stripFilterGate suffix of
          Just (o, t, rest') -> do
            (keep, firstEv, valCursor, afterValue) <- lift (gateTake o t (pushCursor event rest))
            if not keep
              then do
                st' <- emitChunk config (JSONObjectKey key) st
                (st'', after) <- takeValueChunks config valCursor st'
                continue after st''
              else case (rewriter, focusesWhole rest' firstEv) of
                (RewriteDelete, True) -> continue afterValue st
                _ -> do
                  st' <- emitChunk config (JSONObjectKey key) st
                  (st'', after) <- run rest' valCursor st'
                  continue after st''
          Nothing -> case rewriter of
            RewriteDelete
              | focusesWhole suffix event -> do
                  after <- lift (skipValueE (pushCursor event rest))
                  continue after st
              | otherwise -> do
                  st' <- emitChunk config (JSONObjectKey key) st
                  (st'', after) <- run suffix (pushCursor event rest) st'
                  continue after st''
            RewriteTransform _ -> do
              st' <- emitChunk config (JSONObjectKey key) st
              (st'', after) <- run suffix (pushCursor event rest) st'
              continue after st''
