{-# LANGUAGE DeriveFunctor #-}
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
import Data.Scientific (Scientific)
import Data.Text (strip, unpack)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Vector (Vector)
import GHC.Show (ShowS, appPrec)
import HQ.Transformation.Error (TransformationError (..))
import Relude hiding (Const, many, not, or, some, subtract, toStrict)
import Prelude (Show (showsPrec), showParen, showString)

-- | Base functor for transformation expressions over JSON values.
--
-- Transformations read the current JSON value and produce a new one:
-- numeric steps like 'Add' map numbers, string steps like 'ConcatString'
-- map strings, and 'Equal', 'Not' and 'Or' produce booleans. 'Combine'
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
  | -- | @== v@ or @= v@: test the current value for equality with @v@.
    Equal Value
  | -- | @const v@: replace the current value with @v@, whatever it is.
    Const Value
  | -- | @not@: negate the current boolean value.
    Not
  | -- | @a or b@: boolean disjunction of two transformations.
    Or a a
  | -- | @a . b@: apply @b@, then apply @a@ over its result.
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
  Transformation (Fix (Const a)) == Transformation (Fix (Const b)) = a == b
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

-- | @const v@: replace the current value with @v@, whatever it is.
constValue :: Value -> Transformation
constValue = Transformation . Fix . Const

-- | @not@: negate the current boolean value.
not :: Transformation
not = Transformation (Fix Not)

-- | @a or b@: boolean disjunction of two transformations.
or :: Transformation -> Transformation -> Transformation
or (Transformation a) (Transformation b) = Transformation (Fix (Or a b))

-- | @a . b@: apply @b@, then apply @a@ over its result.
combine :: Transformation -> Transformation -> Transformation
combine (Transformation a) (Transformation b) = Transformation (Fix (Combine a b))

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
      Combine left right -> run right value >>= run left

    withNumber :: (Scientific -> Value) -> Value -> Either TransformationError Value
    withNumber apply (Number n) = pure (apply n)
    withNumber _ _ = Left ExpectedNumber

    withString :: (Text -> Value) -> Value -> Either TransformationError Value
    withString apply (String s) = pure (apply s)
    withString _ _ = Left ExpectedString

    withArray :: (Vector Value -> Value) -> Value -> Either TransformationError Value
    withArray apply (Array a) = pure (apply a)
    withArray _ _ = Left ExpectedArray

    withBool :: (Bool -> Value) -> Value -> Either TransformationError Value
    withBool apply (Bool b) = pure (apply b)
    withBool _ _ = Left ExpectedBoolean
