{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner.Rewrite where

import Data.Aeson (Value (..))
import HQ.Early (Early, leave, orLeave)
import HQ.Error (HQError (..))
import HQ.JSON.Encoder (BSStream, EmitState, EncoderConfig, emitChunk, flushEmit)
import HQ.JSON.Event
import HQ.Optic (Optic (..), focusesWhole, prismPredicate)
import HQ.Runner.Cursor
import HQ.Runner.Error (RunnerError (..))
import HQ.Runner.Fold
import HQ.Runner.Rewrite.Emit
import HQ.Transformation (Transformation (Const), runTransformation)
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

runDelete :: Early HQError -> Optic -> EncoderConfig -> RewriteContinuation
runDelete early = runRewrite early RewriteDelete

runOver ::
  Early HQError -> Optic -> Transformation -> EncoderConfig -> RewriteContinuation
runOver early optic transformation =
  runRewrite early (RewriteTransform transformation) optic

-- | Rewrite the document at the cursor, replacing or omitting focused
-- values in place; everything else passes through unchanged.
--
-- Each optic case below is a top-level worker taking its context
-- explicitly (rather than one @where@-closure over the whole query):
-- GHC gives every helper its own worker/wrapper and strictness, and
-- small leaves inline into their callers, instead of fusing all of
-- @over@/@delete@ into a single giant worker.
runRewrite ::
  Early HQError -> Rewriter -> Optic -> EncoderConfig -> RewriteContinuation
runRewrite early rewriter optic config cur0 st0 = do
  RewriteResult st' after <- rewriteRun early rewriter config optic cur0 st0
  st'' <- flushEmit st'
  pure (RewriteResult st'' after)

rewriteRun ::
  Early HQError -> Rewriter -> EncoderConfig -> Optic -> RewriteContinuation
rewriteRun early rewriter config step input st = case step of
  Id -> rewriteValue early rewriter config input st
  _ -> rewriteNavigate early rewriter config step Id input st

rewriteNavigate ::
  Early HQError ->
  Rewriter ->
  EncoderConfig ->
  Optic ->
  Optic ->
  RewriteContinuation
rewriteNavigate early rewriter config step suffix input st = case step of
  Id -> rewriteRun early rewriter config suffix input st
  Compose left right ->
    rewriteNavigate early rewriter config left (right <> suffix) input st
  Field name -> rewriteField early rewriter config name suffix input st
  Each -> rewriteEach early rewriter config suffix input st
  Keys -> rewriteKeys early rewriter config suffix input st
  Values -> rewriteValues early rewriter config suffix input st
  Prism kind ->
    rewritePrism early rewriter config (prismPredicate kind) suffix input st
  PrismJust -> rewriteJust early rewriter config suffix input st
  Ix i -> rewriteIndex early rewriter config i suffix input st
  Filter o t -> rewriteFilter early rewriter config o t suffix input st

-- | Split a member-value suffix with a leading @filter@ step into
-- its gate and remainder, so a kept value deleted as a whole drops
-- its key too.
stripFilterGate :: Optic -> Maybe (Optic, Transformation, Optic)
stripFilterGate (Compose (Filter o t) rest') = Just (o, t, rest')
stripFilterGate (Filter o t) = Just (o, t, Id)
stripFilterGate _ = Nothing

rewriteValue ::
  Early HQError -> Rewriter -> EncoderConfig -> RewriteContinuation
rewriteValue early rewriter config input st = case rewriter of
  RewriteDelete -> RewriteResult st <$> lift (skipValueE early input)
  -- A constant replacement never reads the focused value: skip it
  -- at the text level instead of decoding it.
  RewriteTransform (Const value) -> do
    emitValueChunks config value st =<< lift (skipValueE early input)
  RewriteTransform t -> do
    Materialized v _ rest <- lift (materializeValue early input)
    value <- lift (orLeave early (applyTransformation t v))
    emitValueChunks config value st rest

-- | Emit a transformed value's events as chunks.
emitValueChunks ::
  EncoderConfig -> Value -> EmitState -> Cursor -> BSStream IO RewriteResult
emitValueChunks config value st cursor = go st (valueToEvents value)
  where
    go s [] = pure (RewriteResult s cursor)
    go s (event : events) = do
      s' <- emitChunk config event s
      go s' events

-- | Prism: value whose first event satisfies the predicate goes
-- through the suffix; everything else passes through unchanged.
rewritePrism ::
  Early HQError ->
  Rewriter ->
  EncoderConfig ->
  (JSONEvent -> Bool) ->
  Optic ->
  RewriteContinuation
rewritePrism early rewriter config predicate suffix input st =
  onEventOrEndChunks early input st $ \(Step event rest) ->
    if predicate event
      then rewriteRun early rewriter config suffix (pushCursor event rest) st
      else takeValueChunks early config (pushCursor event rest) st

-- | Prism on non-null: null passes through, anything else goes
-- through the suffix.
rewriteJust ::
  Early HQError -> Rewriter -> EncoderConfig -> Optic -> RewriteContinuation
rewriteJust early rewriter config suffix input st =
  onEventOrEndChunks early input st $ \case
    (Step JSONNull rest) -> do
      st' <- emitChunk config JSONNull st
      pure (RewriteResult st' rest)
    (Step event rest) ->
      rewriteRun early rewriter config suffix (pushCursor event rest) st

-- | Array index: rewrite the element at @index@, pass through the
-- rest of the array. Non-arrays pass through unchanged.
rewriteIndex ::
  Early HQError ->
  Rewriter ->
  EncoderConfig ->
  Int ->
  Optic ->
  RewriteContinuation
rewriteIndex early rewriter config index suffix input st =
  onOpenOrSkipChunks early config input st eventsWithCont
  where
    eventsWithCont =
      [ ( JSONBeginArray,
          \rest s -> go index rest =<< emitChunk config JSONBeginArray s
        )
      ]

    go n stream s = do
      step <- lift (expectArrayStep early stream)
      case step of
        Left rest -> do
          s' <- emitChunk config JSONEndArray s
          pure (RewriteResult s' rest)
        Right (Step event rest)
          | n == 0 -> do
              RewriteResult s' after <- rewriteRun early rewriter config suffix (pushCursor event rest) s
              go (-1) after s'
          | otherwise -> do
              RewriteResult s' after <-
                takeValueChunks early config (pushCursor event rest) s
              go (n - 1) after s'

-- | Object field: rewrite the member named @name@ (dropping the key
-- too under @delete@ when the suffix lands on the value), pass
-- every other member through unchanged.
rewriteField ::
  Early HQError ->
  Rewriter ->
  EncoderConfig ->
  Text ->
  Optic ->
  RewriteContinuation
rewriteField early rewriter config name suffix input st =
  onOpenOrSkipChunks early config input st eventsWithCont
  where
    eventsWithCont =
      [ ( JSONBeginObject,
          \rest s -> do
            st' <- emitChunk config JSONBeginObject s
            traverseObjectChunks early config rest st'
              $ \(ObjectStep key rest') s' ->
                if key == name
                  then rewriteOneMember early rewriter config suffix key rest' s'
                  else emitKeyAndTake early config key rest' s'
        )
      ]

-- | The @each@ traversal: rewrite every array element, or every
-- object member value. Non-containers pass through.
rewriteEach ::
  Early HQError -> Rewriter -> EncoderConfig -> Optic -> RewriteContinuation
rewriteEach early rewriter config suffix input st =
  onOpenOrSkipChunks early config input st eventsWithCont
  where
    eventsWithCont =
      [ ( JSONBeginArray,
          \rest s -> do
            st' <- emitChunk config JSONBeginArray s
            traverseArrayChunks early config rest st' $ \(Step event rest') s' ->
              rewriteRun early rewriter config suffix (pushCursor event rest') s'
        ),
        ( JSONBeginObject,
          \rest s -> do
            st' <- emitChunk config JSONBeginObject s
            traverseObjectChunks early config rest st' $ \(ObjectStep key rest') s' ->
              rewriteOneMember early rewriter config suffix key rest' s'
        )
      ]

-- | The @values@ traversal: rewrite every object member value.
-- Arrays and scalars pass through unchanged (unlike 'rewriteEach').
rewriteValues ::
  Early HQError -> Rewriter -> EncoderConfig -> Optic -> RewriteContinuation
rewriteValues early rewriter config suffix input st =
  onOpenOrSkipChunks early config input st eventsWithCont
  where
    eventsWithCont =
      [ ( JSONBeginObject,
          \rest s -> do
            st' <- emitChunk config JSONBeginObject s
            traverseObjectChunks early config rest st'
              $ \(ObjectStep key rest') s' ->
                rewriteOneMember early rewriter config suffix key rest' s'
        )
      ]

-- | The @filter@ optic: gate the focused value as a whole, running
-- the suffix on kept values and passing dropped values through
-- unchanged.
rewriteFilter ::
  Early HQError ->
  Rewriter ->
  EncoderConfig ->
  Optic ->
  Transformation ->
  Optic ->
  RewriteContinuation
rewriteFilter early rewriter config o t suffix input st =
  onEventOrEndChunks early input st $ \(Step event rest) -> do
    GateResult keep _ valCursor _ <-
      lift $ gateTake early o t $ pushCursor event rest
    if keep
      then rewriteRun early rewriter config suffix valCursor st
      else takeValueChunks early config valCursor st

-- | The @keys@ traversal: rewrite object keys, leaving values alone.
-- Arrays and scalars pass through unchanged (@keys@ is objects-only).
rewriteKeys ::
  Early HQError -> Rewriter -> EncoderConfig -> Optic -> RewriteContinuation
rewriteKeys early rewriter config suffix input st =
  onOpenOrSkipChunks early config input st eventsWithCont
  where
    eventsWithCont =
      [ ( JSONBeginObject,
          \rest s -> do
            st' <- emitChunk config JSONBeginObject s
            traverseObjectChunks early config rest st'
              $ \(ObjectStep key rest') s' -> do
                action <- lift (rewriteKeyText early suffix rewriter key)
                applyKeyAction early config action key rest' s'
        )
      ]

applyKeyAction ::
  Early HQError ->
  EncoderConfig ->
  KeyAction ->
  Text ->
  Cursor ->
  EmitState ->
  BSStream IO RewriteResult
applyKeyAction early config action key rest s = case action of
  DropKey -> RewriteResult s <$> lift (skipValueE early rest)
  KeepKey -> emitKeyAndTake early config key rest s
  RenameKey newKey -> emitKeyAndTake early config newKey rest s

-- | Apply the remainder of a @keys@ optic plus the enclosing
-- 'Rewriter' to one object key. Keys are scalar strings, so the
-- suffix either focuses the key as a whole (checked with
-- 'focusesWhole', e.g. @Id@ or @_String@) or it matches nothing and
-- the key is kept.
rewriteKeyText :: Early HQError -> Optic -> Rewriter -> Text -> IO KeyAction
rewriteKeyText early suffix rw key = case rw of
  RewriteDelete
    | focusesWhole suffix (JSONString key) -> pure DropKey
    | otherwise -> pure KeepKey
  RewriteTransform t
    | focusesWhole suffix (JSONString key) ->
        case runTransformation t (String key) of
          Left err -> leave early $ HQTransformationError err
          Right (String next) ->
            pure (if next == key then KeepKey else RenameKey next)
          Right _ -> leave early $ HQTransformationError KeyNotString
    | otherwise -> pure KeepKey

-- | Rewrite one object member, returning the cursor after it.
-- The key survives unless a @delete@ lands on the member value as
-- a whole. A leading @filter@ step gates the value first: dropped
-- members pass through, and kept members deleted as a whole lose
-- their key too. The object walk itself is shared via
-- 'traverseObjectChunks'.
rewriteOneMember ::
  Early HQError ->
  Rewriter ->
  EncoderConfig ->
  Optic ->
  Text ->
  Cursor ->
  EmitState ->
  BSStream IO RewriteResult
rewriteOneMember early rewriter config suffix key stream st = do
  result <- lift $ pullCursor early stream
  case result of
    Nothing -> lift $ leave early $ HQRunnerError ExpectedMemberValue
    Just (Step event rest) -> case stripFilterGate suffix of
      Just (o, t, rest') ->
        rewriteFilteredMember early rewriter config o t rest' key event rest st
      Nothing ->
        rewritePlainMember early rewriter config suffix key event rest st

rewriteFilteredMember ::
  Early HQError ->
  Rewriter ->
  EncoderConfig ->
  Optic ->
  Transformation ->
  Optic ->
  Text ->
  JSONEvent ->
  Cursor ->
  EmitState ->
  BSStream IO RewriteResult
rewriteFilteredMember early rewriter config o t rest' key event rest st = do
  GateResult keep firstEv valCursor afterValue <-
    lift (gateTake early o t (pushCursor event rest))
  if not keep
    then emitKeyAndTake early config key valCursor st
    else case (rewriter, focusesWhole rest' firstEv) of
      (RewriteDelete, True) -> pure (RewriteResult st afterValue)
      _ -> do
        st' <- emitChunk config (JSONObjectKey key) st
        rewriteRun early rewriter config rest' valCursor st'

rewritePlainMember ::
  Early HQError ->
  Rewriter ->
  EncoderConfig ->
  Optic ->
  Text ->
  JSONEvent ->
  Cursor ->
  EmitState ->
  BSStream IO RewriteResult
rewritePlainMember early rewriter config suffix key event rest st =
  case rewriter of
    RewriteDelete
      | focusesWhole suffix event -> do
          after <- lift (skipValueE early (pushCursor event rest))
          pure (RewriteResult st after)
      | otherwise -> do
          st' <- emitChunk config (JSONObjectKey key) st
          rewriteRun early rewriter config suffix (pushCursor event rest) st'
    RewriteTransform _ -> do
      st' <- emitChunk config (JSONObjectKey key) st
      rewriteRun early rewriter config suffix (pushCursor event rest) st'
