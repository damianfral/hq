{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The transformation DSL over JSON values.
--
-- Transformations are structured as a tree of constructors that describe
-- how to rewrite a JSON value, e.g. @Equal 3 . Add 1@.
module HQ.Transformation where

import Data.Aeson (ToJSON, Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Text (encodeToLazyText)
import Data.Bool qualified (not)
import Data.List qualified as List
import Data.Scientific (Scientific)
import Data.Text (strip, unpack)
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Show (ShowS, appPrec)
import HQ.Transformation.Error (TransformationError (..))
import Relude hiding (Compose, Const, and, isPrefixOf, length, many, not, or, reverse, some, subtract, toStrict, xor)
import Prelude (Show (showsPrec), showParen, showString)

-- | Transformation expressions over JSON values.
--
-- Transformations read the current JSON value and produce a new one:
-- numeric steps like 'Add' map numbers, string steps like 'ConcatString'
-- map strings, and 'Equal', 'Not' and 'Or' produce booleans. 'Compose'
-- composes two transformations right-to-left: @a . b@ applies @b@ first,
-- then applies @a@ over its result.
data Transformation
  = -- | @+ n@: add a number to the current value.
    Add Scientific
  | -- | @* n@: multiply the current value by a number.
    Multiply Scientific
  | -- | @- n@: subtract a number from the current value.
    Subtract Scientific
  | -- | @/ n@: divide the current value by a number.
    Divide Scientific
  | -- | @< n@: test whether the current number is less than @n@.
    Lt Scientific
  | -- | @<= n@: test whether the current number is at most @n@.
    Lte Scientific
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
  | -- | @merge o@: shallow-merge the object operand @o@ into the
    -- current object value; @o@ wins member conflicts (like jq @+@).
    -- Nested objects are replaced wholesale, not merged.
    Merge (KeyMap.KeyMap Value)
  | -- | @deepMerge o@: recursively merge the object operand @o@ into
    -- the current object value (like jq @*@). Conflicting members
    -- that are both objects merge recursively; anything else takes
    -- the operand's value.
    DeepMerge (KeyMap.KeyMap Value)
  | -- | @sort@: sort the elements of an array value in canonical order
    -- (null, booleans, numbers, strings, arrays, objects). Stable:
    -- equal elements keep their input order.
    ArraySort
  | -- | @== v@ or @= v@: test the current value for equality with @v@.
    Equal Value
  | -- | @const v@: replace the current value with @v@, whatever it is.
    Const Value
  | -- | @not@: negate the current boolean value.
    Not
  | -- | @a or b@: boolean disjunction of two transformations.
    Or Transformation Transformation
  | -- | @a and b@: boolean conjunction of two transformations.
    And Transformation Transformation
  | -- | @a xor b@: boolean exclusive disjunction of two transformations.
    Xor Transformation Transformation
  | -- | @a . b@: apply @b@, then apply @a@ over its result.
    Compose Transformation Transformation
  deriving (Eq)

instance Show Transformation where
  showsPrec d (Add n) =
    showParen (d > appPrec) $ showString "+" . showsJson (Number n)
  showsPrec d (Multiply n) =
    showParen (d > appPrec) $ showString "*" . showsJson (Number n)
  showsPrec d (Subtract n) =
    showParen (d > appPrec) $ showString "-" . showsJson (Number n)
  showsPrec d (Divide n) =
    showParen (d > appPrec) $ showString "/" . showsJson (Number n)
  showsPrec d (Lt n) =
    showParen (d > appPrec) $ showString "<" . showsJson (Number n)
  showsPrec d (Lte n) =
    showParen (d > appPrec) $ showString "<=" . showsJson (Number n)
  showsPrec d (ConcatString s) =
    showParen (d > appPrec) $ showString "++ " . showsPrec (appPrec + 1) s
  showsPrec d (ConcatArray els) =
    showParen (d > appPrec) $ showString "concat " . showsJson els
  showsPrec _ Trim = showString "trim"
  showsPrec d (Replace a b) =
    showParen (d > appPrec)
      $ showString "replace "
      . showsPrec (appPrec + 1) a
      . showString " "
      . showsPrec (appPrec + 1) b
  showsPrec d (StripPrefix p) =
    showParen (d > appPrec) $ showString "stripPrefix " . showsPrec (appPrec + 1) p
  showsPrec d (StripSuffix s) =
    showParen (d > appPrec) $ showString "stripSuffix " . showsPrec (appPrec + 1) s
  showsPrec d (IsPrefixOf p) =
    showParen (d > appPrec) $ showString "isPrefixOf " . showsPrec (appPrec + 1) p
  showsPrec d (IsSuffixOf s) =
    showParen (d > appPrec) $ showString "isSuffixOf " . showsPrec (appPrec + 1) s
  showsPrec d (IsInfixOf i) =
    showParen (d > appPrec) $ showString "isInfixOf " . showsPrec (appPrec + 1) i
  showsPrec _ IsEmpty = showString "isEmpty"
  showsPrec _ ArrayLength = showString "length"
  showsPrec _ ArrayReverse = showString "reverse"
  showsPrec _ ArrayUnique = showString "unique"
  showsPrec d (Merge members) =
    showParen (d > appPrec) $ showString "merge " . showsJson (Object members)
  showsPrec d (DeepMerge members) =
    showParen (d > appPrec) $ showString "deepMerge " . showsJson (Object members)
  showsPrec _ ArraySort = showString "sort"
  showsPrec d (Equal v) =
    showParen (d > appPrec) $ showString "== " . showsJson v
  showsPrec d (Const v) =
    showParen (d > appPrec) $ showString "const " . showsJson v
  showsPrec _ Not = showString "not"
  showsPrec d (Or a b) =
    showParen (d > orPrec)
      $ showsPrec orPrec a
      . showString " or "
      . showsPrec (orPrec + 1) b
    where
      orPrec = 4
  showsPrec d (And a b) =
    showParen (d > andPrec)
      $ showsPrec andPrec a
      . showString " and "
      . showsPrec (andPrec + 1) b
    where
      andPrec = 6
  showsPrec d (Xor a b) =
    showParen (d > xorPrec)
      $ showsPrec xorPrec a
      . showString " xor "
      . showsPrec (xorPrec + 1) b
    where
      xorPrec = 5
  showsPrec d (Compose a b) =
    showParen (d > composePrec)
      $ showsPrec composePrec a
      . showString " . "
      . showsPrec (composePrec + 1) b
    where
      composePrec = 7

-- | Render an embedded JSON value in compact DSL syntax, e.g. @3@ or @[1,2]@.
showsJson :: (ToJSON a) => a -> ShowS
showsJson = showString . unpack . toStrict . encodeToLazyText

-- | Apply a transformation to a JSON value.
--
-- Every step is total over 'Value': a step that does not fit the current
-- value (for example adding to a string) fails with a description of the
-- expected value type.  Composition applies right-to-left, matching the
-- @a . b@ DSL syntax: @b@ runs first, then @a@ over its result.
runTransformation :: Transformation -> Value -> Either TransformationError Value
runTransformation step value = case step of
  Add n -> withNumber (Number . (+ n)) value
  Multiply n -> withNumber (Number . (* n)) value
  Subtract n -> withNumber (Number . (+ negate n)) value
  Divide n -> withNumber (Number . (/ n)) value
  Lt n -> withNumber (Bool . (< n)) value
  Lte n -> withNumber (Bool . (<= n)) value
  ConcatString suffix -> withString (String . (<> suffix)) value
  ConcatArray elements -> withArray (Array . (<> fromList elements)) value
  Trim -> withString (String . strip) value
  Replace needle replacement ->
    withString (String . T.replace needle replacement) value
  StripPrefix prefix ->
    withString (String . stripAffix T.stripPrefix prefix) value
  StripSuffix suffix ->
    withString (String . stripAffix T.stripSuffix suffix) value
  IsPrefixOf prefix -> withString (Bool . T.isPrefixOf prefix) value
  IsSuffixOf suffix -> withString (Bool . T.isSuffixOf suffix) value
  IsInfixOf infix_ -> withString (Bool . T.isInfixOf infix_) value
  IsEmpty -> withArray (Bool . V.null) value
  ArrayLength -> withArray (Number . fromIntegral . V.length) value
  ArrayReverse -> withArray (Array . V.reverse) value
  ArrayUnique -> withArray (Array . fromList . List.nub . toList) value
  Merge members -> withObject (Object . KeyMap.union members) value
  DeepMerge members -> withObject (\o -> Object (KeyMap.unionWith deepMergeValues o members)) value
  ArraySort -> withArray (Array . fromList . List.sortBy compareValues . toList) value
  Equal literal -> pure (Bool (value == literal))
  Const v -> pure v
  Not -> withBool (Bool . Data.Bool.not) value
  Or left right -> do
    result <- runTransformation left value
    case result of
      Bool b
        | b -> pure result
        | otherwise -> runTransformation right value
      _ -> Left OrBranchNotBoolean
  And left right -> do
    result <- runTransformation left value
    case result of
      Bool b
        | b -> runTransformation right value
        | otherwise -> pure result
      _ -> Left AndBranchNotBoolean
  Xor left right -> do
    l <- runTransformation left value
    r <- runTransformation right value
    case (l, r) of
      (Bool a, Bool b) -> pure (Bool (a /= b))
      _ -> Left XorBranchNotBoolean
  Compose left right -> runTransformation right value >>= runTransformation left
  where
    -- Eliminate one JSON shape: rebuild on match, fail with the
    -- shape's error otherwise. Every leaf transformation goes through
    -- here, so a new shape adds a projector, not a helper.
    with :: (Value -> Maybe a) -> TransformationError -> (a -> Value) -> Value -> Either TransformationError Value
    with project err build v = maybe (Left err) (pure . build) (project v)

    asNumber :: Value -> Maybe Scientific
    asNumber (Number n) = Just n
    asNumber _ = Nothing

    asString :: Value -> Maybe Text
    asString (String s) = Just s
    asString _ = Nothing

    -- \| Strip an affix when present, leaving the value unchanged otherwise.
    stripAffix :: (Text -> Text -> Maybe Text) -> Text -> Text -> Text
    stripAffix stripF affix s = fromMaybe s (stripF affix s)

    asArray :: Value -> Maybe (Vector Value)
    asArray (Array a) = Just a
    asArray _ = Nothing

    asObject :: Value -> Maybe (KeyMap.KeyMap Value)
    asObject (Object o) = Just o
    asObject _ = Nothing

    asBool :: Value -> Maybe Bool
    asBool (Bool b) = Just b
    asBool _ = Nothing

    -- Short names, kept as synonyms of 'with'.
    withNumber :: (Scientific -> Value) -> Value -> Either TransformationError Value
    withNumber = with asNumber ExpectedNumber

    withString :: (Text -> Value) -> Value -> Either TransformationError Value
    withString = with asString ExpectedString

    withArray :: (Vector Value -> Value) -> Value -> Either TransformationError Value
    withArray = with asArray ExpectedArray

    withObject :: (KeyMap.KeyMap Value -> Value) -> Value -> Either TransformationError Value
    withObject = with asObject ExpectedObject

    withBool :: (Bool -> Value) -> Value -> Either TransformationError Value
    withBool = with asBool ExpectedBoolean

-- | Recursive object merge with right-wins conflicts (like jq @*@):
-- members that are objects on both sides merge recursively, anything
-- else takes the replacement value.
deepMergeValues :: Value -> Value -> Value
deepMergeValues (Object current) (Object replacement) =
  Object (KeyMap.unionWith deepMergeValues current replacement)
deepMergeValues _ replacement = replacement

-- | Canonical ordering of JSON values, matching jq's @sort@:
-- null, then booleans, numbers, strings, arrays, objects. Numbers
-- compare numerically, strings lexicographically, arrays
-- element-wise (a proper prefix sorts first), objects by their
-- key/value pairs with keys in order. Total: any two values compare.
compareValues :: Value -> Value -> Ordering
compareValues x y = case compare (valueRank x) (valueRank y) of
  EQ -> compareSame x y
  other -> other
  where
    compareSame Null Null = EQ
    compareSame (Bool a) (Bool b) = compare a b
    compareSame (Number a) (Number b) = compare a b
    compareSame (String a) (String b) = compare a b
    compareSame (Array a) (Array b) =
      lexCompare compareValues (V.toList a) (V.toList b)
    compareSame (Object a) (Object b) =
      lexCompare comparePair (sorted a) (sorted b)
      where
        sorted = List.sortOn fst . KeyMap.toList
        comparePair (k1, v1) (k2, v2) = compare k1 k2 <> compareValues v1 v2
    compareSame a b = compare (valueRank a) (valueRank b)

-- | Lexicographic comparison; a proper prefix sorts first.
lexCompare :: (a -> a -> Ordering) -> [a] -> [a] -> Ordering
lexCompare _ [] [] = EQ
lexCompare _ [] _ = LT
lexCompare _ _ [] = GT
lexCompare c (x : xs) (y : ys) = c x y <> lexCompare c xs ys

-- | Type rank for 'compareValues': jq's cross-type order.
valueRank :: Value -> Int
valueRank Null = 0
valueRank (Bool False) = 1
valueRank (Bool True) = 2
valueRank (Number _) = 3
valueRank (String _) = 4
valueRank (Array _) = 5
valueRank (Object _) = 6
