{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Optic where

import Data.Fix
import GHC.Show (Show (showsPrec), appPrec, showParen, showString, shows)
import HQ.JSON.Event (JSONEvent, isArray, isBool, isNull, isNumber, isObject, isString)
import HQ.Transformation (Transformation (..), TransformationF (..))
import Relude hiding (Compose, Const, filter, id, many, some)

-- | JSON value shapes selectable by a prism.
data PrismKind
  = PString
  | PNumber
  | PBool
  | PNull
  | PArray
  | PObject
  deriving (Eq, Show)

-- | Base functor for optic paths over JSON values. 'Field', 'Ix' and
-- '_Just' are affine; 'Id' is a lens; 'Each', 'Keys', 'Values' are
-- traversals; '_String'-style constructors are prisms; 'Filter' is
-- affine. See 'OpticType' for the cardinality lattice.
data OpticF a
  = -- | Focus on a named field of a JSON object. Fails on non-objects.
    Field Text
  | -- | Traverse all elements of a JSON array, or the values of an object.
    -- Composed with another optic, it distributes that optic over each element:
    -- @@users.each.@name@ focuses on the @name@ field of each array element.
    Each
  | -- | Traverse object keys as string values (objects only).
    -- Focuses on each member name (@JSONString@) in document order.
    -- Arrays and scalars focus on nothing. Rewriting through @keys@
    -- renames object members.
    Keys
  | -- | Traverse object member values (objects only, unlike 'Each').
    -- Arrays and scalars focus on nothing; use 'Each' or 'Ix' for arrays.
    Values
  | -- | The identity optic: focuses on the whole value unchanged.
    Id
  | -- | Sequence two optics: first focus where the left points,
    --   then within each target, focus where the right points.
    Compose a a
  | -- | Prism: focus on a JSON value of the given shape. Fails on
    -- mismatching values.
    Prism PrismKind
  | -- | Focus on any non-null JSON value. Fails on null.
    -- Matching-only (affine traversal, not a lawful prism).
    PrismJust
  | -- | Focus on the element at the given index of a JSON array (arrays
    -- only; objects and scalars focus on nothing). Out-of-bounds and
    -- negative indices focus on nothing.
    Ix Int
  | -- | Keep the focused value when the transformation holds of the
    -- sub-optic's focus: @filter o p@ focuses its input if @p@ maps
    -- some value focused by @o@ to true, and focuses nothing otherwise.
    -- Focusing is existential ('any'): one passing sub-value keeps the
    -- whole input. Affine traversal (zero or one of its input).
    Filter a Transformation
  deriving (Eq, Show, Functor)

-- | An optic path over JSON values.
newtype Optic = Optic {unOptic :: Fix OpticF}

instance Eq Optic where
  Optic (Fix (Field a)) == Optic (Fix (Field b)) = a == b
  Optic (Fix Each) == Optic (Fix Each) = True
  Optic (Fix Id) == Optic (Fix Id) = True
  Optic (Fix (Compose a b)) == Optic (Fix (Compose c d)) =
    Optic a == Optic c && Optic b == Optic d
  Optic (Fix (Prism a)) == Optic (Fix (Prism b)) = a == b
  Optic (Fix PrismJust) == Optic (Fix PrismJust) = True
  Optic (Fix Keys) == Optic (Fix Keys) = True
  Optic (Fix Values) == Optic (Fix Values) = True
  Optic (Fix (Ix a)) == Optic (Fix (Ix b)) = a == b
  Optic (Fix (Filter o1 t1)) == Optic (Fix (Filter o2 t2)) =
    Optic o1 == Optic o2 && t1 == t2
  _ == _ = False

instance Show Optic where
  showsPrec d (Optic (Fix (Field name))) =
    showParen (d > appPrec) $ showString "@" . showsPrec (appPrec + 1) name
  showsPrec _ (Optic (Fix Each)) = showString "each"
  showsPrec _ (Optic (Fix Keys)) = showString "keys"
  showsPrec _ (Optic (Fix Values)) = showString "values"
  showsPrec _ (Optic (Fix Id)) = showString "id"
  showsPrec d (Optic (Fix (Compose a b))) =
    showParen (d > composePrec)
      $ showsPrec prec (Optic a)
      . showString " . "
      . showsPrec prec (Optic b)
    where
      composePrec = 5
      prec = composePrec + 1
  showsPrec _ (Optic (Fix (Prism kind))) = showString (prismName kind)
  showsPrec _ (Optic (Fix PrismJust)) = showString "_Just"
  showsPrec _ (Optic (Fix (Ix i))) = showString $ "ix " <> show i
  showsPrec d (Optic (Fix (Filter o t))) =
    showParen (d > filterPrec)
      $ showString "filter "
      . showsArgs o t
    where
      filterPrec = appPrec
      -- Single atoms stay bare (@filter @age == 30@); anything longer
      -- goes in one paren group (@filter (each . @age == 30)@).
      showsArgs oo tt
        | isAtomOptic oo && isAtomTrans tt =
            showsPrec (filterPrec + 1) (Optic oo)
              . showString " "
              . showsPrec (filterPrec + 1) tt
        | otherwise =
            showString "("
              . shows (Optic oo)
              . showString " "
              . shows tt
              . showString ")"
      isAtomOptic (Fix (Compose _ _)) = False
      isAtomOptic _ = True
      isAtomTrans (Transformation (Fix (Combine _ _))) = False
      isAtomTrans (Transformation (Fix (Or _ _))) = False
      isAtomTrans _ = True

-- | Focus on a named field of a JSON object (affine traversal).
field :: Text -> Optic
field = Optic . Fix . Field

-- | Traverse all elements of a JSON array, or all values of an object.
each :: Optic
each = Optic (Fix Each)

-- | Traverse object keys as strings (objects only).
keys :: Optic
keys = Optic (Fix Keys)

-- | Traverse object member values (objects only).
values :: Optic
values = Optic (Fix Values)

-- | The identity optic: focuses on the whole value.
id :: Optic
id = Optic (Fix Id)

-- | Compose two optics sequentially.
compose :: Optic -> Optic -> Optic
compose (Optic a) (Optic b) = Optic (Fix (Compose a b))

_String :: Optic
_String = Optic (Fix (Prism PString))

_Number :: Optic
_Number = Optic (Fix (Prism PNumber))

_Bool :: Optic
_Bool = Optic (Fix (Prism PBool))

_Null :: Optic
_Null = Optic (Fix (Prism PNull))

_Array :: Optic
_Array = Optic (Fix (Prism PArray))

_Object :: Optic
_Object = Optic (Fix (Prism PObject))

-- | Non-null focus ('Just' case); affine, not a lawful prism.
_Just :: Optic
_Just = Optic (Fix PrismJust)

-- | Focus on the element at the given index of a JSON array.
ix :: Int -> Optic
ix = Optic . Fix . Ix

-- | First-event predicate for each type prism; total over 'PrismKind'.
prismPredicate :: PrismKind -> JSONEvent -> Bool
prismPredicate PString = isString
prismPredicate PNumber = isNumber
prismPredicate PBool = isBool
prismPredicate PNull = isNull
prismPredicate PArray = isArray
prismPredicate PObject = isObject

-- | Compose with 'Id' elimination.
appendOptic :: Fix OpticF -> Fix OpticF -> Fix OpticF
appendOptic (Fix Id) rest = rest
appendOptic step rest = Fix (Compose step rest)

-- | Whether a suffix focuses the whole value starting at @event@.
focusesWhole :: Fix OpticF -> JSONEvent -> Bool
focusesWhole (Fix opticF) event = case opticF of
  Id -> True
  Field _ -> False
  Each -> False
  Keys -> False
  Values -> False
  Ix _ -> False
  Prism kind -> prismPredicate kind event
  PrismJust -> not (isNull event)
  Filter _ _ -> False
  Compose l r -> focusesWhole l event && focusesWhole r event

prismName :: PrismKind -> String
prismName PString = "_String"
prismName PNumber = "_Number"
prismName PBool = "_Bool"
prismName PNull = "_Null"
prismName PArray = "_Array"
prismName PObject = "_Object"

-- | Keep the focused value when the transformation holds of the
-- sub-optic's focus (@filter o p@ focuses its input when @p@ maps some
-- value focused by @o@ to true). For example,
-- @each . filter @age == 30@ focuses the array elements having an
-- @age@ field equal to 30, while @filter (each . @age == 30)@ keeps
-- whole documents containing such a value. Single atoms stay bare;
-- anything longer goes in one paren group.
filter :: Optic -> Transformation -> Optic
filter o t = Optic (Fix (Filter (unOptic o) t))
