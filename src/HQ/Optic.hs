{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Optic where

import GHC.Show (Show (showsPrec), appPrec, showParen, showString, shows)
import HQ.JSON.Event (JSONEvent, isArray, isBool, isNull, isNumber, isObject, isString)
import HQ.Transformation (Transformation)
import HQ.Transformation qualified as Trans
import Relude hiding (Compose, Const, filter, id, many, some)

-- | JSON value shapes selectable by a prism.
data PrismKind
  = PString
  | PNumber
  | PBool
  | PNull
  | PArray
  | PObject
  deriving stock (Eq, Show)

-- | Optic paths over JSON values. 'Field', 'Ix' and
-- '_Just' are affine; 'Id' is a lens; 'Each', 'Keys', 'Values' are
-- traversals; '_String'-style constructors are prisms; 'Filter' is
-- affine. See 'OpticType' for the cardinality lattice.
data Optic
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
    Compose Optic Optic
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
    Filter Optic Transformation
  deriving stock (Eq)

instance Show Optic where
  showsPrec d (Field name) =
    showParen (d > appPrec) $ showString "@" . showsPrec (appPrec + 1) name
  showsPrec _ Each = showString "each"
  showsPrec _ Keys = showString "keys"
  showsPrec _ Values = showString "values"
  showsPrec _ Id = showString "id"
  showsPrec d (Compose a b) =
    showParen (d > composePrec)
      $ showsPrec prec a
      . showString " . "
      . showsPrec prec b
    where
      composePrec :: Int = 5
      prec = composePrec + 1
  showsPrec _ (Prism kind) = showString (prismName kind)
  showsPrec _ PrismJust = showString "_Just"
  showsPrec _ (Ix i) = showString $ "ix " <> show i
  showsPrec d (Filter o t) =
    showParen (d > filterPrec)
      $ showString "filter "
      . showsArgs o t
    where
      filterPrec = appPrec
      -- Single atoms stay bare (@filter @age == 30@); anything longer
      -- goes in one paren group (@filter (each . @age == 30)@).
      showsArgs oo tt
        | isAtomOptic oo && isAtomTrans tt =
            showsPrec (filterPrec + 1) oo
              . showString " "
              . showsPrec (filterPrec + 1) tt
        | otherwise =
            showString "("
              . shows oo
              . showString " "
              . shows tt
              . showString ")"
      isAtomOptic (Compose _ _) = False
      isAtomOptic _ = True
      -- NOTE: 'Trans.Compose' is the transformation sequencing node;
      -- bare 'Compose' here would be 'Optic.Compose' (the local
      -- definition shadows the import), so the qualifier is load-bearing.
      isAtomTrans (Trans.Compose _ _) = False
      isAtomTrans (Trans.Or _ _) = False
      isAtomTrans (Trans.And _ _) = False
      isAtomTrans (Trans.Xor _ _) = False
      isAtomTrans _ = True

-- | First-event predicate for each type prism; total over 'PrismKind'.
prismPredicate :: PrismKind -> JSONEvent -> Bool
prismPredicate PString = isString
prismPredicate PNumber = isNumber
prismPredicate PBool = isBool
prismPredicate PNull = isNull
prismPredicate PArray = isArray
prismPredicate PObject = isObject

-- | Compose with 'Id' elimination on either side: @( '<>' )@ with
-- 'Id' as identity. Reassociation preserves focusing semantics (both
-- runners induct through the continuation), though structural 'Eq'
-- still distinguishes nestings.
appendOptic :: Optic -> Optic -> Optic
appendOptic Id rest = rest
appendOptic step Id = step
appendOptic step rest = Compose step rest

instance Semigroup Optic where (<>) = appendOptic

instance Monoid Optic where mempty = Id

-- | Whether a suffix focuses the whole value starting at @event@.
focusesWhole :: Optic -> JSONEvent -> Bool
focusesWhole optic event = case optic of
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
