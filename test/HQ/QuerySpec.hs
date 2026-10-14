{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.QuerySpec (spec) where

import Data.Aeson (Value (..))
import HQ.Optic
import HQ.Query
import HQ.Transformation hiding (Compose)
import HQ.Transformation qualified as T
import HQ.Transformation.AST (TransformationTypeError (..))
import HQ.Transformation.TransformationType (ValueType (..))
import Relude hiding (Compose, filter, id, or)
import Test.Syd

spec :: Spec
spec = describe "HQ.Query" $ do
  describe "typecheckQuery" $ do
    it "accepts well-typed queries" $ forM_ acceptCases $ \q ->
      typecheckQuery q `shouldBe` Right q

    it "rejects focus mismatches" $ forM_ focusCases $ \(q, focus, expected) ->
      case typecheckQuery q of
        Left (InvalidFocusType _ _ focus' expected') -> do
          focus' `shouldBe` focus
          expected' `shouldBe` expected
        Left err -> expectationFailure $ "wrong error: " <> show err
        Right _ -> expectationFailure "expected a type error"

    it "rejects transformation mismatches" $ forM_ transformationCases $ \(q, expected) ->
      case typecheckQuery q of
        Left (InvalidTransformationType err) -> err `shouldBe` expected
        Left err -> expectationFailure $ "wrong error: " <> show err
        Right _ -> expectationFailure "expected a type error"

acceptCases :: [Query]
acceptCases =
  [ Fold (Field "name"),
    Preview (Field "name"),
    Preview (Prism PString),
    Preview Id,
    Preview Each,
    Preview (Compose Each (Field "x")),
    Preview (Compose Each (Filter (Field "age") (Equal (Number 29)))),
    Over (Compose Each (Prism PNumber)) (Add 1),
    Over (Field "age") (Add 1),
    Delete (Ix 0),
    Fold Keys,
    Over Each (T.Compose (Equal (Number 1)) Trim),
    Fold (Filter (Field "a") (Equal (Number 1))),
    Preview (Filter (Field "a") (Equal (Number 1)))
  ]

focusCases :: [(Query, ValueType, ValueType)]
focusCases =
  [ (Over (Compose Each (Prism PNumber)) (ConcatString "!"), ValueNumber, ValueString),
    (Fold (Filter (Prism PNumber) (IsPrefixOf "a")), ValueNumber, ValueString)
  ]

transformationCases :: [(Query, TransformationTypeError)]
transformationCases =
  [ (Over Each (T.Compose (Add 1) (Equal (Number 3))), InvalidCompose (T.Compose (Add 1) (Equal (Number 3))) ValueBool ValueNumber),
    (Over Each (Or (Add 1) (Equal (Number 2))), InvalidOr (Or (Add 1) (Equal (Number 2))) ValueNumber),
    (Fold (Filter (Field "a") (Add 1)), InvalidFilter (Add 1) ValueNumber),
    (Fold (Filter (Field "a") (T.Compose (Add 1) (Equal (Number 3)))), InvalidCompose (T.Compose (Add 1) (Equal (Number 3))) ValueBool ValueNumber),
    (Fold (Filter (Compose Each (Filter (Field "b") (Add 2))) (Equal (Number 1))), InvalidFilter (Add 2) ValueNumber)
  ]
