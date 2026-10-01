{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The transformation DSL over JSON values.
--
-- Transformations are structured as a tree of constructors that describe
-- how to rewrite a JSON value (see 'TransformationF') and are built with the
-- smart constructors in this module, e.g. @equal 3 . add 1@.
module HQ.Transformation where

import Data.Aeson (ToJSON, Value (..))
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Bool (not)
import Data.Fix
import Data.Functor.Classes (Eq1)
import qualified Data.List as List
import Data.Scientific (Scientific)
import Data.Text (strip, unpack)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic1, Generically1 (..))
import GHC.Show (ShowS, appPrec)
import HQ.Transformation.Error (TransformationError (..))
import Relude hiding (Compose, Const, and, isPrefixOf, length, many, not, or, reverse, some, subtract, toStrict, xor)
import Prelude (Show (showsPrec), showParen, showString)

-- | Base functor for transformation expressions over JSON values.
--
-- Transformations read the current JSON value and produce a new one:
-- numeric steps like 'Add' map numbers, string steps like 'ConcatString'
-- map strings, and 'Equal', 'Not' and 'Or' produce booleans. 'Compose'
-- composes two transformations right-to-left: @a . b@ applies @b@ first,
-- then applies @a@ over its result.
data TransformationF a
  = -- | @+ n@: add a number to the current value.
    Add Scientific
  | -- | @* n@: multiply the current value by a number.
    Multiply Scientific
  | -- | @- n@: subtract a number from the current value.
    Subtract Scientific
  | -- | @/ n@: divide the current value by a number.
    Divide Scientific
  | -- | @++ s@: append a string to a string value.
    ConcatString Text
  | -- | @concat [..]@: append the given elements to an array value.
    ConcatArray [Value]
  | -- | @trim@: strip surrounding whitespace from a string value.
    Trim
  | -- | @replace a b@: replace occurrences of @a@ with @b@ in a string value.
    Replace Text Text
  | -- | @stripPrefix p@: remove the prefix @p@ from a string value, if present.
    StripPrefix Text
  | -- | @stripSuffix s@: remove the suffix @s@ from a string value, if present.
    StripSuffix Text
  | -- | @isPrefixOf p@: test whether a string value starts with @p@.
    IsPrefixOf Text
  | -- | @isSuffixOf s@: test whether a string value ends with @s@.
    IsSuffixOf Text
  | -- | @isInfixOf i@: test whether a string value contains @i@.
    IsInfixOf Text
  | -- | @isEmpty@: test whether an array value is empty.
    IsEmpty
  | -- | @length@: the length of an array value, as a number.
    ArrayLength
  | -- | @reverse@: reverse an array value.
    ArrayReverse
  | -- | @unique@: drop duplicate elements of an array value, keeping first occurrences.
    ArrayUnique
  | -- | @== v@ or @= v@: test the current value for equality with @v@.
    Equal Value
  | -- | @const v@: replace the current value with @v@, whatever it is.
    Const Value
  | -- | @not@: negate the current boolean value.
    Not
  | -- | @a or b@: boolean disjunction of two transformations.
    Or a a
  | -- | @a and b@: boolean conjunction of two transformations.
    And a a
  | -- | @a xor b@: boolean exclusive disjunction of two transformations.
    Xor a a
  | -- | @a . b@: apply @b@, then apply @a@ over its result.
    Compose a a
  deriving (Eq, Show, Functor, Generic1)
  deriving (Eq1) via Generically1 TransformationF

-- | A transformation expression over JSON values.
newtype Transformation = Transformation (Fix TransformationF) deriving (Eq)

instance Show Transformation where
  showsPrec d (Transformation (Fix (Add n))) =
    showParen (d > appPrec) $ showString "+" . showsJson (Number n)
  showsPrec d (Transformation (Fix (Multiply n))) =
    showParen (d > appPrec) $ showString "*" . showsJson (Number n)
  showsPrec d (Transformation (Fix (Subtract n))) =
    showParen (d > appPrec) $ showString "-" . showsJson (Number n)
  showsPrec d (Transformation (Fix (Divide n))) =
    showParen (d > appPrec) $ showString "/" . showsJson (Number n)
  showsPrec d (Transformation (Fix (ConcatString s))) =
    showParen (d > appPrec) $ showString "++ " . showsPrec (appPrec + 1) s
  showsPrec d (Transformation (Fix (ConcatArray els))) =
    showParen (d > appPrec) $ showString "concat " . showsJson els
  showsPrec _ (Transformation (Fix Trim)) = showString "trim"
  showsPrec d (Transformation (Fix (Replace a b))) =
    showParen (d > appPrec)
      $ showString "replace "
      . showsPrec (appPrec + 1) a
      . showString " "
      . showsPrec (appPrec + 1) b
  showsPrec d (Transformation (Fix (StripPrefix p))) =
    showParen (d > appPrec) $ showString "stripPrefix " . showsPrec (appPrec + 1) p
  showsPrec d (Transformation (Fix (StripSuffix s))) =
    showParen (d > appPrec) $ showString "stripSuffix " . showsPrec (appPrec + 1) s
  showsPrec d (Transformation (Fix (IsPrefixOf p))) =
    showParen (d > appPrec) $ showString "isPrefixOf " . showsPrec (appPrec + 1) p
  showsPrec d (Transformation (Fix (IsSuffixOf s))) =
    showParen (d > appPrec) $ showString "isSuffixOf " . showsPrec (appPrec + 1) s
  showsPrec d (Transformation (Fix (IsInfixOf i))) =
    showParen (d > appPrec) $ showString "isInfixOf " . showsPrec (appPrec + 1) i
  showsPrec _ (Transformation (Fix IsEmpty)) = showString "isEmpty"
  showsPrec _ (Transformation (Fix ArrayLength)) = showString "length"
  showsPrec _ (Transformation (Fix ArrayReverse)) = showString "reverse"
  showsPrec _ (Transformation (Fix ArrayUnique)) = showString "unique"
  showsPrec d (Transformation (Fix (Equal v))) =
    showParen (d > appPrec) $ showString "== " . showsJson v
  showsPrec d (Transformation (Fix (Const v))) =
    showParen (d > appPrec) $ showString "const " . showsJson v
  showsPrec _ (Transformation (Fix Not)) = showString "not"
  showsPrec d (Transformation (Fix (Or a b))) =
    showParen (d > orPrec)
      $ showsPrec orPrec (Transformation a)
      . showString " or "
      . showsPrec (orPrec + 1) (Transformation b)
    where
      orPrec = 4
  showsPrec d (Transformation (Fix (And a b))) =
    showParen (d > andPrec)
      $ showsPrec andPrec (Transformation a)
      . showString " and "
      . showsPrec (andPrec + 1) (Transformation b)
    where
      andPrec = 6
  showsPrec d (Transformation (Fix (Xor a b))) =
    showParen (d > xorPrec)
      $ showsPrec xorPrec (Transformation a)
      . showString " xor "
      . showsPrec (xorPrec + 1) (Transformation b)
    where
      xorPrec = 5
  showsPrec d (Transformation (Fix (Compose a b))) =
    showParen (d > composePrec)
      $ showsPrec composePrec (Transformation a)
      . showString " . "
      . showsPrec (composePrec + 1) (Transformation b)
    where
      composePrec = 7

-- | Render an embedded JSON value in compact DSL syntax, e.g. @3@ or @[1,2]@.
showsJson :: (ToJSON a) => a -> ShowS
showsJson = showString . unpack . toStrict . encodeToLazyText

-- | @+ n@: add a number to the current value.
add :: Scientific -> Transformation
add = Transformation . Fix . Add

-- | @* n@: multiply the current value by a number.
multiply :: Scientific -> Transformation
multiply = Transformation . Fix . Multiply

-- | @- n@: subtract a number from the current value.
subtract :: Scientific -> Transformation
subtract = Transformation . Fix . Subtract

-- | @/ n@: divide the current value by a number.
divide :: Scientific -> Transformation
divide = Transformation . Fix . Divide

-- | @++ s@: append a string to a string value.
concatString :: Text -> Transformation
concatString = Transformation . Fix . ConcatString

-- | @concat [..]@: append the given elements to an array value.
concatArray :: [Value] -> Transformation
concatArray = Transformation . Fix . ConcatArray

-- | @trim@: strip surrounding whitespace from a string value.
trim :: Transformation
trim = Transformation (Fix Trim)

-- | @replace a b@: replace occurrences of @a@ with @b@ in a string value.
replace :: Text -> Text -> Transformation
replace a b = Transformation (Fix (Replace a b))

-- | @stripPrefix p@: remove the prefix @p@ from a string value, if present.
stripPrefix :: Text -> Transformation
stripPrefix = Transformation . Fix . StripPrefix

-- | @stripSuffix s@: remove the suffix @s@ from a string value, if present.
stripSuffix :: Text -> Transformation
stripSuffix = Transformation . Fix . StripSuffix

-- | @isPrefixOf p@: test whether a string value starts with @p@.
isPrefixOf :: Text -> Transformation
isPrefixOf = Transformation . Fix . IsPrefixOf

-- | @isSuffixOf s@: test whether a string value ends with @s@.
isSuffixOf :: Text -> Transformation
isSuffixOf = Transformation . Fix . IsSuffixOf

-- | @isInfixOf i@: test whether a string value contains @i@.
isInfixOf :: Text -> Transformation
isInfixOf = Transformation . Fix . IsInfixOf

-- | @isEmpty@: test whether an array value is empty.
isEmpty :: Transformation
isEmpty = Transformation (Fix IsEmpty)

-- | @length@: the length of an array value, as a number.
length :: Transformation
length = Transformation (Fix ArrayLength)

-- | @reverse@: reverse an array value.
reverse :: Transformation
reverse = Transformation (Fix ArrayReverse)

-- | @unique@: drop duplicate elements of an array value, keeping first occurrences.
unique :: Transformation
unique = Transformation (Fix ArrayUnique)

-- | @== v@ or @= v@: test the current value for equality with @v@.
equal :: Value -> Transformation
equal = Transformation . Fix . Equal

-- | @const v@: replace the current value with @v@, whatever it is.
constValue :: Value -> Transformation
constValue = Transformation . Fix . Const

-- | @not@: negate the current boolean value.
not :: Transformation
not = Transformation (Fix Not)

-- | @a or b@: boolean disjunction of two transformations.
or :: Transformation -> Transformation -> Transformation
or (Transformation a) (Transformation b) = Transformation (Fix (Or a b))

-- | @a and b@: boolean conjunction of two transformations.
and :: Transformation -> Transformation -> Transformation
and (Transformation a) (Transformation b) = Transformation (Fix (And a b))

-- | @a xor b@: boolean exclusive disjunction of two transformations.
xor :: Transformation -> Transformation -> Transformation
xor (Transformation a) (Transformation b) = Transformation (Fix (Xor a b))

-- | @a . b@: apply @b@, then apply @a@ over its result.
compose :: Transformation -> Transformation -> Transformation
compose (Transformation a) (Transformation b) = Transformation (Fix (Compose a b))

-- | Apply a transformation to a JSON value.
--
-- Every step is total over 'Value': a step that does not fit the current
-- value (for example adding to a string) fails with a description of the
-- expected value type.  Composition applies right-to-left, matching the
-- @a . b@ DSL syntax: @b@ runs first, then @a@ over its result.
runTransformation :: Transformation -> Value -> Either TransformationError Value
runTransformation (Transformation transformation) = run transformation
  where
    run :: Fix TransformationF -> Value -> Either TransformationError Value
    run (Fix step) value = case step of
      Add n -> withNumber (Number . (+ n)) value
      Multiply n -> withNumber (Number . (* n)) value
      Subtract n -> withNumber (Number . (+ negate n)) value
      Divide n -> withNumber (Number . (/ n)) value
      ConcatString suffix -> withString (String . (<> suffix)) value
      ConcatArray elements -> withArray (Array . (<> fromList elements)) value
      Trim -> withString (String . strip) value
      Replace needle replacement -> withString (String . T.replace needle replacement) value
      StripPrefix prefix -> withString (String . stripAffix T.stripPrefix prefix) value
      StripSuffix suffix -> withString (String . stripAffix T.stripSuffix suffix) value
      IsPrefixOf prefix -> withString (Bool . T.isPrefixOf prefix) value
      IsSuffixOf suffix -> withString (Bool . T.isSuffixOf suffix) value
      IsInfixOf infix_ -> withString (Bool . T.isInfixOf infix_) value
      IsEmpty -> withArray (Bool . V.null) value
      ArrayLength -> withArray (Number . fromIntegral . V.length) value
      ArrayReverse -> withArray (Array . V.reverse) value
      ArrayUnique -> withArray (Array . fromList . List.nub . toList) value
      Equal literal -> pure (Bool (value == literal))
      Const v -> pure v
      Not -> withBool (Bool . Data.Bool.not) value
      Or left right -> do
        result <- run left value
        case result of
          Bool b
            | b -> pure result
            | otherwise -> run right value
          _ -> Left OrBranchNotBoolean
      And left right -> do
        result <- run left value
        case result of
          Bool b
            | b -> run right value
            | otherwise -> pure result
          _ -> Left AndBranchNotBoolean
      Xor left right -> do
        l <- run left value
        r <- run right value
        case (l, r) of
          (Bool a, Bool b) -> pure (Bool (a /= b))
          _ -> Left XorBranchNotBoolean
      Compose left right -> run right value >>= run left

    withNumber :: (Scientific -> Value) -> Value -> Either TransformationError Value
    withNumber apply (Number n) = pure (apply n)
    withNumber _ _ = Left ExpectedNumber

    withString :: (Text -> Value) -> Value -> Either TransformationError Value
    withString apply (String s) = pure (apply s)
    withString _ _ = Left ExpectedString

    -- \| Strip an affix when present, leaving the value unchanged otherwise.
    stripAffix :: (Text -> Text -> Maybe Text) -> Text -> Text -> Text
    stripAffix stripF affix s = fromMaybe s (stripF affix s)

    withArray :: (Vector Value -> Value) -> Value -> Either TransformationError Value
    withArray apply (Array a) = pure (apply a)
    withArray _ _ = Left ExpectedArray

    withBool :: (Bool -> Value) -> Value -> Either TransformationError Value
    withBool apply (Bool b) = pure (apply b)
    withBool _ _ = Left ExpectedBoolean
