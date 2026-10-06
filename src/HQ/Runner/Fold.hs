{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner.Fold where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Vector as Vector
import HQ.Early (Early, leave, orLeave)
import HQ.Error (HQError (..))
import HQ.JSON.Decoder (initialDecoder)
import HQ.JSON.Event
import HQ.Optic (Optic (..), PrismKind, prismPredicate)
import HQ.Runner.Cursor (Continuation, Cursor (..), expectArrayStep, expectObjectStep, onEventOrEnd, pushCursor, skipMemberValue, skipRestOfArray, skipRestOfObject, skipValue, traverseArray, traverseObject)
import HQ.Runner.Error (RunnerError (..))
import HQ.Runner.Take (takeFirstValue, takeValue)
import HQ.Transformation (Transformation, runTransformation)
import HQ.Transformation.Error (TransformationError (..))
import Relude hiding (Compose, id, many, some, state)
import Streaming (Of (..))
import qualified Streaming.Prelude as S

-- | Materialize the value at the cursor into events and a 'Value',
-- returning the value, its events and the cursor after it.
-- Shared by folding and rewriting so @filter@ gates and @over@
-- replacements agree on error mapping.
materializeValue :: Early HQError -> Cursor -> IO (Value, [JSONEvent], Cursor)
materializeValue early cur = do
  events :> afterValue <- S.toList (takeValue early cur)
  v <- orLeave early $ first HQRunnerError $ eventsToValue events
  case events of
    [] -> leave early $ HQRunnerError EmptyValue
    _ -> pure (v, events, afterValue)

-- | Test a @filter@ gate, mapping failures to 'HQError'.
-- Shared by 'runFold' and 'runRewrite'.
gateValue :: Optic -> Transformation -> Value -> Either HQError Bool
gateValue o t v =
  first HQTransformationError $ evalFilterGate o t v

-- | Test a @filter@ gate on a streamed value, returning whether it is
-- kept plus replay/after cursors. Shared by 'runFold' and 'runRewrite'
-- so both agree on gating and cursor rebuild.
gateTake ::
  Early HQError ->
  Optic ->
  Transformation ->
  Cursor ->
  IO (Bool, JSONEvent, Cursor, Cursor)
gateTake early o t cur = do
  (v, events, afterValue) <- materializeValue early cur
  keep <- orLeave early (gateValue o t v)
  case events of
    [] -> leave early $ HQRunnerError EmptyValue
    (firstEv : _) ->
      let Cursor _ dec txt = afterValue
       in pure (keep, firstEv, Cursor events dec txt, afterValue)

-- | Apply a transformation, mapping failures to 'HQError'.
-- Shared by rewriting (@over@) replacements.
applyTransformation :: Transformation -> Value -> Either HQError Value
applyTransformation t v =
  first HQTransformationError $ runTransformation t v

-- | Interpret an optic against the JSON value at the cursor.
--
-- The continuation receives the cursor positioned immediately after
-- the value currently being interpreted.
runFold :: Early HQError -> Optic -> Continuation
runFold early optic = run optic (takeValue early)
  where
    run :: Optic -> Continuation -> Continuation
    run opticF k input = case opticF of
      Id -> k input
      Field name -> runField name k input
      Each -> runEach k input
      Keys -> runKeys k input
      Values -> runValues k input
      Filter o t -> runFilter o t k input
      Compose left right -> run left (run right k) input
      Prism kind -> runScalar (prismPredicate kind) k input
      PrismJust -> runJust k input
      Ix i -> runIndex i k input

    runField :: Text -> Continuation -> Continuation
    runField name k input =
      onEventOrEnd early input $ \case
        (JSONBeginObject, rest) -> findField name k rest
        (event, rest) -> skipValue early (pushCursor event rest)

    findField :: Text -> Continuation -> Continuation
    findField name k = go
      where
        go stream = do
          step <- lift (expectObjectStep early stream)
          case step of
            Left rest -> pure rest
            Right (key, rest)
              | key == name -> k rest >>= skipRestOfObject early
              | otherwise -> skipMemberValue early rest >>= go

    ----------------------------------------------------------------
    -- Each
    ----------------------------------------------------------------

    runEach :: Continuation -> Continuation
    runEach k input =
      onEventOrEnd early input $ \case
        (JSONBeginArray, rest) ->
          traverseArray early rest $ \(event, rest') -> k (pushCursor event rest')
        (JSONBeginObject, rest) ->
          traverseObject early rest $ \(_, rest') -> k rest'
        (event, rest) -> skipValue early (pushCursor event rest)

    ----------------------------------------------------------------
    -- Keys: object keys as strings (objects only)
    ----------------------------------------------------------------

    runKeys :: Continuation -> Continuation
    runKeys k input =
      onEventOrEnd early input $ \case
        (JSONBeginObject, rest) ->
          traverseObject early rest $ \(key, rest') -> do
            _ <- k (Cursor [JSONString key] initialDecoder (pure ()))
            skipMemberValue early rest'
        (event, rest) -> skipValue early (pushCursor event rest)

    ----------------------------------------------------------------
    -- Values: object member values only (arrays focus on nothing)
    ----------------------------------------------------------------

    runValues :: Continuation -> Continuation
    runValues k input =
      onEventOrEnd early input $ \case
        (JSONBeginObject, rest) ->
          traverseObject early rest $ \(_, rest') -> k rest'
        (event, rest) -> skipValue early (pushCursor event rest)

    ----------------------------------------------------------------
    -- Filter: keep the value when the predicate holds of the
    -- sub-optic's focus (existential over the focused values)
    ----------------------------------------------------------------

    runFilter :: Optic -> Transformation -> Continuation -> Continuation
    runFilter o t k input =
      onEventOrEnd early input $ \(event, rest0) -> do
        (keep, _, replay, rest) <- lift (gateTake early o t (pushCursor event rest0))
        if keep
          then k replay
          else pure rest

    ----------------------------------------------------------------
    -- Scalar prisms
    ----------------------------------------------------------------

    runScalar :: (JSONEvent -> Bool) -> Continuation -> Continuation
    runScalar predicate k input =
      onEventOrEnd early input $ \(event, rest) ->
        if predicate event
          then k (pushCursor event rest)
          else skipValue early (pushCursor event rest) -- not a match; consume anyway

    ----------------------------------------------------------------
    -- PrismJust
    ----------------------------------------------------------------

    runJust :: Continuation -> Continuation
    runJust k input =
      onEventOrEnd early input $ \case
        (JSONNull, rest) -> pure rest
        (event, rest) -> k (pushCursor event rest)

    ----------------------------------------------------------------
    -- Array index
    ----------------------------------------------------------------

    runIndex :: Int -> Continuation -> Continuation
    runIndex index k input =
      onEventOrEnd early input $ \case
        (JSONBeginArray, rest) -> findIndex index rest
        (event, rest) -> skipValue early (pushCursor event rest)
      where
        findIndex n stream
          | n < 0 = skipRestOfArray early stream
          | otherwise = do
              step <- lift (expectArrayStep early stream)
              case step of
                Left rest -> pure rest
                Right (event, rest) -> case n of
                  0 -> k (pushCursor event rest) >>= skipRestOfArray early
                  _ -> skipValue early (pushCursor event rest) >>= findIndex (n - 1)

--------------------------------------------------------------------------------
-- Pure navigation: the same optic semantics over in-memory values.
--
-- Navigation here is total: absent members, out-of-bounds indices and
-- prism mismatches focus on nothing (mirroring the streaming runners'
-- skip behavior). Only @filter@ predicates are partial, and their
-- failures propagate.
--------------------------------------------------------------------------------

-- | Focus an optic on an in-memory value, collecting every focused
-- value. Used to evaluate @filter@ gates without re-driving the
-- streaming decoder.
focusMany :: Optic -> Value -> Either TransformationError [Value]
focusMany o v = case o of
  Field name -> pure (lookupField name v)
  Each -> pure (eachValues v)
  Keys -> pure (keyValues v)
  Values -> pure (valuesValues v)
  Id -> pure [v]
  Compose l r -> do
    ls <- focusMany l v
    concat <$> traverse (focusMany r) ls
  Prism kind -> pure $ matchPrism kind v
  PrismJust -> pure $ matchJust v
  Ix i -> pure $ matchIndex i v
  Filter o' t -> do
    keep <- evalFilterGate o' t v
    pure [v | keep]

-- | Evaluate a @filter@ gate on an in-memory value: true when the
-- transformation maps some focused sub-value to true.
evalFilterGate :: Optic -> Transformation -> Value -> Either TransformationError Bool
evalFilterGate o t v = focusMany o v >>= anyMatch t
  where
    anyMatch :: Transformation -> [Value] -> Either TransformationError Bool
    anyMatch _ [] = pure False
    anyMatch t' (w : ws) = do
      b <- testValue t' w
      if b then pure True else anyMatch t' ws
    testValue :: Transformation -> Value -> Either TransformationError Bool
    testValue t' w = case runTransformation t' w of
      Left err -> Left err
      Right (Bool b) -> pure b
      Right _ -> Left FilterNotBoolean

-- | Object member lookup: missing members and non-objects focus on
-- nothing.
lookupField :: Text -> Value -> [Value]
lookupField name v = case v of
  Object o -> maybeToList (KeyMap.lookup (Key.fromText name) o)
  _ -> []

-- | Object member values; arrays and scalars focus on nothing.
objectValues :: Value -> [Value]
objectValues v = case v of
  Object o -> KeyMap.elems o
  _ -> []

-- | Array elements plus object member values.
eachValues :: Value -> [Value]
eachValues v = case v of
  Array a -> Vector.toList a
  Object _ -> objectValues v
  _ -> []

-- | Object keys as strings; arrays focus on nothing.
keyValues :: Value -> [Value]
keyValues v = case v of
  Object o -> map (String . Key.toText) (KeyMap.keys o)
  _ -> []

-- | Object member values only; arrays focus on nothing.
valuesValues :: Value -> [Value]
valuesValues = objectValues

-- | Match a type prism against an in-memory value, reusing the shared
-- 'prismPredicate' on the value's first event (every value yields at
-- least one event, forced lazily).
matchPrism :: PrismKind -> Value -> [Value]
matchPrism kind v = case valueToEvents v of
  (ev : _) | prismPredicate kind ev -> [v]
  _ -> []

matchJust :: Value -> [Value]
matchJust v = case v of
  Null -> []
  _ -> [v]

matchIndex :: Int -> Value -> [Value]
matchIndex i v = case v of
  Array a -> maybeToList (a Vector.!? i)
  _ -> []

-- | Execute an optic, emitting at most one value: the first one it
-- selects.
--
-- The fold is short-circuited: once the first value has been emitted,
-- no further input is read from the source. This is library-level
-- first-match semantics over any optic; the @preview@ CLI command
-- narrows its input to at-most-one optics via 'typecheckQuery'.
runPreview :: Early HQError -> Optic -> Continuation
runPreview early optic input = do
  takeFirstValue (runFold early optic input)
  pure input -- the after-cursor is meaningless for preview; nobody reads it
