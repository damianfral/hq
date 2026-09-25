{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Optic where

import Data.Fix
import GHC.Show (appPrec)
import Relude hiding (Compose, id, many, some)
import Prelude (Show (showsPrec), showParen, showString)

-- | Base functor for optic paths over JSON values.
--
-- Optics are structured as a tree of constructors that describe
-- how to focus into a JSON value. 'Field', 'Ix' and '_Just' are
-- single-or-no-target optics (affine traversals); 'Id' is single-target
-- (lens); 'Each', 'Keys' and 'Values' are multi-target optics
-- (traversals); the remaining '_String'-style constructors are prisms.
--
-- 'Null' plays the role of 'Nothing': '_Null' is the lawful prism for
-- the null case, while '_Just' matches any non-null value. '_Just' is
-- matching-only (its identity 'review' is not a section on 'Null'),
-- hence classified affine rather than prism.
data OpticF a
  = -- | Focus on a named field of a JSON object. Fails on non-objects.
    Field Text
  | -- | Traverse all elements of a JSON array, or the values of an object.
    -- Composed with another optic, it distributes that optic over each element:
    -- @#users.each.#name@ focuses on the @name@ field of each array element.
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
  | -- | Prism: focus on a JSON String value. Fails on non-strings.
    PrismString
  | -- | Prism: focus on a JSON Number value. Fails on non-numbers.
    PrismNumber
  | -- | Prism: focus on a JSON Bool value. Fails on non-booleans.
    PrismBool
  | -- | Prism: focus on a JSON null value. Fails on non-nulls.
    PrismNull
  | -- | Prism: focus on a JSON Array value. Fails on non-arrays.
    PrismArray
  | -- | Prism: focus on a JSON Object value. Fails on non-objects.
    PrismObject
  | -- | Focus on any non-null JSON value. Fails on null.
    -- Matching-only (affine traversal, not a lawful prism).
    PrismJust
  | -- | Focus on the element at the given index of a JSON array (arrays
    -- only; objects and scalars focus on nothing). Out-of-bounds and
    -- negative indices focus on nothing.
    Ix Int
  deriving (Eq, Ord, Show, Functor)

-- | An optic path over JSON values.
newtype Optic = Optic {unOptic :: Fix OpticF}

instance Eq Optic where
  Optic (Fix (Field a)) == Optic (Fix (Field b)) = a == b
  Optic (Fix Each) == Optic (Fix Each) = True
  Optic (Fix Id) == Optic (Fix Id) = True
  Optic (Fix (Compose a b)) == Optic (Fix (Compose c d)) =
    Optic a == Optic c && Optic b == Optic d
  Optic (Fix PrismString) == Optic (Fix PrismString) = True
  Optic (Fix PrismNumber) == Optic (Fix PrismNumber) = True
  Optic (Fix PrismBool) == Optic (Fix PrismBool) = True
  Optic (Fix PrismNull) == Optic (Fix PrismNull) = True
  Optic (Fix PrismArray) == Optic (Fix PrismArray) = True
  Optic (Fix PrismObject) == Optic (Fix PrismObject) = True
  Optic (Fix PrismJust) == Optic (Fix PrismJust) = True
  Optic (Fix Keys) == Optic (Fix Keys) = True
  Optic (Fix Values) == Optic (Fix Values) = True
  Optic (Fix (Ix a)) == Optic (Fix (Ix b)) = a == b
  _ == _ = False

instance Show Optic where
  showsPrec d (Optic (Fix (Field name))) =
    showParen (d > appPrec) $ showString "#" . showsPrec (appPrec + 1) name
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
  showsPrec _ (Optic (Fix PrismString)) = showString "_String"
  showsPrec _ (Optic (Fix PrismNumber)) = showString "_Number"
  showsPrec _ (Optic (Fix PrismBool)) = showString "_Bool"
  showsPrec _ (Optic (Fix PrismNull)) = showString "_Null"
  showsPrec _ (Optic (Fix PrismArray)) = showString "_Array"
  showsPrec _ (Optic (Fix PrismObject)) = showString "_Object"
  showsPrec _ (Optic (Fix PrismJust)) = showString "_Just"
  showsPrec _ (Optic (Fix (Ix i))) = showString $ "ix " <> show i

-- | Focus on a named field of a JSON object (affine traversal).
field :: Text -> Optic
field = Optic . Fix . Field

-- | Traverse all elements of a JSON array, or all values of an object.
-- Composed with another optic, it distributes that optic over each element:
-- @#users.each.#name@ focuses on the @name@ field of each array element.
each :: Optic
each = Optic (Fix Each)

-- | Traverse object keys as strings (objects only).
-- @keys@ on @{"a":1}@ focuses @"a"@; arrays focus on nothing.
keys :: Optic
keys = Optic (Fix Keys)

-- | Traverse object member values (objects only).
-- @values@ on @{"a":1}@ focuses @1@; arrays focus on nothing.
values :: Optic
values = Optic (Fix Values)

-- | The identity optic: focuses on the whole value.
-- @id@ is the unit of optic composition: @compose id o = o@ and @compose o id = o@.
id :: Optic
id = Optic (Fix Id)

-- | Compose two optics sequentially.
-- @compose l r@ first focuses where @l@ points, then within each target,
-- focuses where @r@ points.
compose :: Optic -> Optic -> Optic
compose (Optic a) (Optic b) = Optic (Fix (Compose a b))

-- | Prism: focus on a JSON String value.
_String :: Optic
_String = Optic (Fix PrismString)

-- | Prism: focus on a JSON Number value.
_Number :: Optic
_Number = Optic (Fix PrismNumber)

-- | Prism: focus on a JSON Bool value.
_Bool :: Optic
_Bool = Optic (Fix PrismBool)

-- | Prism: focus on a JSON null value. This is the lawful prism for
-- the null ('Nothing') case; see the '_Just' affine traversal for
-- the non-null case.
_Null :: Optic
_Null = Optic (Fix PrismNull)

-- | Prism: focus on a JSON Array value.
_Array :: Optic
_Array = Optic (Fix PrismArray)

-- | Prism: focus on a JSON Object value.
_Object :: Optic
_Object = Optic (Fix PrismObject)

-- | Focus on any non-null JSON value (the 'Just' case; see 'PrismNull'
-- for the null case). Matching-only: usable for folding and rewriting,
-- but classified as an affine traversal rather than a prism because its
-- identity 'review' is not a section on 'Null'.
_Just :: Optic
_Just = Optic (Fix PrismJust)

-- | Focus on the element at the given index of a JSON array (arrays
-- only; objects and scalars focus on nothing).
ix :: Int -> Optic
ix = Optic . Fix . Ix
