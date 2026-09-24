{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner.Fold where

import Control.Monad.Error.Class (MonadError (throwError))
import Data.Fix (Fix (..))
import HQ.JSON.Event
import HQ.Optic (Optic (..), OpticF (..))
import HQ.Runner.Cursor (Cursor (..), K, ValueStreamF, pullCursor, pushCursor, skipMemberValue, skipValue)
import HQ.Runner.Take (takeFirstValue, takeValue)
import Relude hiding (Compose, id, many, some, state)

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

-- | Execute an optic, emitting at most one value: the first one it
-- selects (lens @preview@ semantics).
--
-- The fold is short-circuited: once the first value has been emitted,
-- no further input is read from the source.
runPreview :: Optic -> K
runPreview optic input = do
  takeFirstValue (runFold optic input)
  pure input -- the after-cursor is meaningless for preview; nobody reads it
