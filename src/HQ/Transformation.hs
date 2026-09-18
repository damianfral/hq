{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The transformation DSL over JSON values.
--
-- Transformations are structured as a tree of constructors that describe
-- how to rewrite a JSON value (see 'TransformationF') and are built with the
-- smart constructors in this module, e.g. @add 1 . equal 3@.
module HQ.Transformation where

import Data.Aeson (ToJSON, Value (..))
import Data.Aeson.Text (encodeToLazyText)
import Data.Fix
import Data.Scientific (Scientific)
import Data.Text (unpack)
import Data.Text.Lazy (toStrict)
import GHC.Show (ShowS, appPrec)
import Relude hiding (many, not, or, some, subtract, toStrict)
import Prelude (Show (showsPrec), showParen, showString)

-- | Base functor for transformation expressions over JSON values.
--
-- Transformations read the current JSON value and produce a new one:
-- numeric steps like 'Add' map numbers, string steps like 'ConcatString'
-- map strings, and 'Equal', 'Not' and 'Or' produce booleans. 'Combine'
-- applies one transformation and passes its result to the next, like a
-- @.@ pipeline.
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
  | -- | @== v@ or @= v@: test the current value for equality with @v@.
    Equal Value
  | -- | @not@: negate the current boolean value.
    Not
  | -- | @a or b@: boolean disjunction of two transformations.
    Or a a
  | -- | @a . b@: apply @a@, then apply @b@ to its result.
    Combine a a
  deriving (Eq, Show, Functor)

-- | A transformation expression over JSON values.
newtype Transformation = Transformation (Fix TransformationF)

instance Eq Transformation where
  Transformation (Fix (Add a)) == Transformation (Fix (Add b)) = a == b
  Transformation (Fix (Multiply a)) == Transformation (Fix (Multiply b)) = a == b
  Transformation (Fix (Subtract a)) == Transformation (Fix (Subtract b)) = a == b
  Transformation (Fix (Divide a)) == Transformation (Fix (Divide b)) = a == b
  Transformation (Fix (ConcatString a)) == Transformation (Fix (ConcatString b)) = a == b
  Transformation (Fix (ConcatArray a)) == Transformation (Fix (ConcatArray b)) = a == b
  Transformation (Fix Trim) == Transformation (Fix Trim) = True
  Transformation (Fix (Replace a b)) == Transformation (Fix (Replace c d)) = a == c && b == d
  Transformation (Fix (Equal a)) == Transformation (Fix (Equal b)) = a == b
  Transformation (Fix Not) == Transformation (Fix Not) = True
  Transformation (Fix (Or a b)) == Transformation (Fix (Or c d)) =
    Transformation a == Transformation c && Transformation b == Transformation d
  Transformation (Fix (Combine a b)) == Transformation (Fix (Combine c d)) =
    Transformation a == Transformation c && Transformation b == Transformation d
  _ == _ = False

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
  showsPrec d (Transformation (Fix (Equal v))) =
    showParen (d > appPrec) $ showString "== " . showsJson v
  showsPrec _ (Transformation (Fix Not)) = showString "not"
  showsPrec d (Transformation (Fix (Or a b))) =
    showParen (d > orPrec)
      $ showsPrec orPrec (Transformation a)
      . showString " or "
      . showsPrec (orPrec + 1) (Transformation b)
    where
      orPrec = 4
  showsPrec d (Transformation (Fix (Combine a b))) =
    showParen (d > composePrec)
      $ showsPrec composePrec (Transformation a)
      . showString " . "
      . showsPrec (composePrec + 1) (Transformation b)
    where
      composePrec = 5

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

-- | @== v@ or @= v@: test the current value for equality with @v@.
equal :: Value -> Transformation
equal = Transformation . Fix . Equal

-- | @not@: negate the current boolean value.
not :: Transformation
not = Transformation (Fix Not)

-- | @a or b@: boolean disjunction of two transformations.
or :: Transformation -> Transformation -> Transformation
or (Transformation a) (Transformation b) = Transformation (Fix (Or a b))

-- | @a . b@: apply @a@, then apply @b@ to its result.
combine :: Transformation -> Transformation -> Transformation
combine (Transformation a) (Transformation b) = Transformation (Fix (Combine a b))
