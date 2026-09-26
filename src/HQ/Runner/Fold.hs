{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner.Fold where

import Control.Monad.Error.Class (MonadError (throwError))
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Fix (Fix (..))
import qualified Data.Vector as Vector
import HQ.JSON.Decoder (initialDecoder)
import HQ.JSON.Event
import HQ.Optic (Optic (..), OpticF (..), PrismKind, prismPredicate)
import HQ.Runner.Cursor (Continuation, Cursor (..), EventStream, pullCursor, pushCursor, skipMemberValue, skipValue)
import HQ.Runner.Take (takeFirstValue, takeValue)
import HQ.Transformation (Transformation, runTransformation)
import Relude hiding (Compose, id, many, some, state)
import Streaming (Of (..))
import qualified Streaming.Prelude as S

-- | Interpret an optic against the JSON value at the cursor.
--
-- The continuation receives the cursor positioned immediately after
-- the value currently being interpreted.
runFold :: Optic -> Continuation
runFold (Optic optic) = run optic takeValue
  where
    run :: Fix OpticF -> Continuation -> Continuation
    run (Fix opticF) k input = case opticF of
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
    runField name k input = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure input
        Just (JSONBeginObject, rest) -> findField name k rest
        Just (event, rest) -> skipValue (pushCursor event rest)

    findField :: Text -> Continuation -> Continuation
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
    skipRestOfObject :: Cursor -> EventStream Cursor
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

    runEach :: Continuation -> Continuation
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
    -- Keys: object keys as strings (objects only)
    ----------------------------------------------------------------

    runKeys :: Continuation -> Continuation
    runKeys k input = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure input
        Just (JSONBeginObject, rest) -> keysObject rest
        Just (event, rest) -> skipValue (pushCursor event rest)
      where
        keysObject :: Cursor -> EventStream Cursor
        keysObject stream = do
          result <- lift (pullCursor stream)
          case result of
            Nothing -> throwError "unexpected end of input while reading object"
            Just (JSONEndObject, rest) -> pure rest
            Just (JSONObjectKey key, rest) -> do
              _ <- k (Cursor [JSONString key] initialDecoder (pure ()))
              afterValue <- skipMemberValue rest
              keysObject afterValue
            Just _ -> throwError "invalid JSON object"

    ----------------------------------------------------------------
    -- Values: object member values only (arrays focus on nothing)
    ----------------------------------------------------------------

    runValues :: Continuation -> Continuation
    runValues k input = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure input
        Just (JSONBeginObject, rest) -> valuesObject rest
        Just (event, rest) -> skipValue (pushCursor event rest)
      where
        valuesObject stream = do
          result <- lift (pullCursor stream)
          case result of
            Nothing -> throwError "unexpected end of input while reading object"
            Just (JSONEndObject, rest) -> pure rest
            Just (JSONObjectKey _, rest) -> do
              afterValue <- k rest -- cursor is already at the value
              valuesObject afterValue
            Just _ -> throwError "invalid JSON object"

    ----------------------------------------------------------------
    -- Filter: keep the value when the predicate holds of the
    -- sub-optic's focus (existential over the focused values)
    ----------------------------------------------------------------

    runFilter :: Fix OpticF -> Transformation -> Continuation -> Continuation
    runFilter o t k input = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure input
        Just (event, rest0) -> do
          events :> rest <- lift (S.toList (takeValue (pushCursor event rest0)))
          keep <- case eventsToValue events of
            Left err -> throwError err
            Right v -> case evalFilterGate o t v of
              Left err -> throwError err
              Right b -> pure b
          if keep
            then k (Cursor events (cursorDecoder rest) (cursorText rest))
            else pure rest
      where
        cursorDecoder (Cursor _ dec _) = dec
        cursorText (Cursor _ _ txt) = txt

    ----------------------------------------------------------------
    -- Scalar prisms
    ----------------------------------------------------------------

    runScalar :: (JSONEvent -> Bool) -> Continuation -> Continuation
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

    runJust :: Continuation -> Continuation
    runJust k input = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> pure input
        Just (JSONNull, rest) -> pure rest
        Just (event, rest) -> k (pushCursor event rest)

    ----------------------------------------------------------------
    -- Array index
    ----------------------------------------------------------------

    runIndex :: Int -> Continuation -> Continuation
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
    skipRestOfArray :: Cursor -> EventStream Cursor
    skipRestOfArray input = do
      result <- lift (pullCursor input)
      case result of
        Nothing -> throwError "unexpected end of input while reading array"
        Just (JSONEndArray, rest) -> pure rest
        Just (event, rest) -> do
          afterValue <- skipValue (pushCursor event rest)
          skipRestOfArray afterValue

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
focusMany :: Optic -> Value -> Either Text [Value]
focusMany (Optic optic) = focusFix optic
  where
    focusFix :: Fix OpticF -> Value -> Either Text [Value]
    focusFix (Fix f) v = case f of
      Field name -> pure (lookupField name v)
      Each -> pure (eachValues v)
      Keys -> pure (keyValues v)
      Values -> pure (valuesValues v)
      Id -> pure [v]
      Compose l r -> do
        ls <- focusFix l v
        concat <$> traverse (focusFix r) ls
      Prism kind -> pure (matchPrism kind v)
      PrismJust -> pure (matchJust v)
      Ix i -> pure (matchIndex i v)
      Filter o t -> do
        keep <- evalFilterGate o t v
        pure [v | keep]

-- | Evaluate a @filter@ gate on an in-memory value: true when the
-- transformation maps some focused sub-value to true.
evalFilterGate :: Fix OpticF -> Transformation -> Value -> Either Text Bool
evalFilterGate o t v = focusMany (Optic o) v >>= anyMatch t
  where
    anyMatch :: Transformation -> [Value] -> Either Text Bool
    anyMatch _ [] = pure False
    anyMatch t' (w : ws) = do
      b <- testValue t' w
      if b then pure True else anyMatch t' ws
    testValue :: Transformation -> Value -> Either Text Bool
    testValue t' w = case runTransformation t' w of
      Left err -> Left err
      Right (Bool b) -> pure b
      Right _ -> Left "filter transformation must produce a boolean"

-- | Object member lookup: missing members and non-objects focus on
-- nothing.
lookupField :: Text -> Value -> [Value]
lookupField name v = case v of
  Object o -> maybeToList (KeyMap.lookup (Key.fromText name) o)
  _ -> []

-- | Array elements plus object member values.
eachValues :: Value -> [Value]
eachValues v = case v of
  Array a -> Vector.toList a
  Object o -> KeyMap.elems o
  _ -> []

-- | Object keys as strings; arrays focus on nothing.
keyValues :: Value -> [Value]
keyValues v = case v of
  Object o -> map (String . Key.toText) (KeyMap.keys o)
  _ -> []

-- | Object member values only; arrays focus on nothing.
valuesValues :: Value -> [Value]
valuesValues v = case v of
  Object o -> KeyMap.elems o
  _ -> []

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
runPreview :: Optic -> Continuation
runPreview optic input = do
  takeFirstValue (runFold optic input)
  pure input -- the after-cursor is meaningless for preview; nobody reads it
