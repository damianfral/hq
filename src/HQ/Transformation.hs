{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Transformation where

import Data.Aeson (Value)
import Data.Fix (Fix)
import Data.Scientific (Scientific)
import Data.Vector (Vector)
import Relude

data Arity = Unary | Binary

data TransformationF f
  = -- Number → Number
    Addition Scientific
  | Subtraction Scientific
  | Multiplication Scientific
  | Division Scientific
  | -- String → String
    Concatenation Text
  | Lowercase Text
  | Uppercase Text
  | Trim
  | Replace Text Text
  | StripPrefix Text
  | StripSuffix Text
  | -- Array → Array
    ArrayConcatenation (Vector Value)
  | Reverse (Vector Value)
  | Flatten (Vector (Vector Value))
  | -- Various → Number
    Length (Vector Value) Scientific
  | -- Predicates
    Equal Value Bool
  | LT Scientific Bool
  | LTE Scientific Bool
  | GT Scientific Bool
  | GTE Scientific Bool
  | IsPrefixOf Text Bool
  | IsSuffixOf Text Bool
  | IsInfixOf Text Bool
  | -- Boolean → Boolean
    Not f
  | And f f
  | Or f f
  | -- Transformation combinator
    Flip f
  | Combine f f

type Transformation = Fix TransformationF
