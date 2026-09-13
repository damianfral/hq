{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Optic where

import Data.Fix
import GHC.Show (appPrec)
import Relude hiding (Compose, id, many, some)
import Prelude (Show (showsPrec), showParen, showString)

-- | Base functor for optic paths over JSON values.
--
-- Optics are structured as a tree of constructors that describe
-- how to focus into a JSON value. 'Field' and 'Id' are single-target
-- optics (lenses/affine traversals); 'Each' and 'Every' are
-- multi-target optics (traversals).
data OpticF a
  = -- | Focus on a named field of a JSON object. Fails on non-objects.
    Field Text
  | -- | Traverse all elements of a JSON array, or the values of an object.
    -- Composed with another optic, it distributes that optic over each element:
    -- @#users.each.#name@ focuses on the @name@ field of each array element.
    Each
  | -- | The identity optic: focuses on the whole value unchanged.
    Id
  | -- | Sequence two optics: first focus where the left points,
    --   then within each target, focus where the right points.
    Compose a a
  deriving (Eq, Ord, Show, Functor)

-- | An optic path over JSON values.
newtype Optic = Optic (Fix OpticF)

instance Eq Optic where
  Optic (Fix (Field a)) == Optic (Fix (Field b)) = a == b
  Optic (Fix Each) == Optic (Fix Each) = True
  Optic (Fix Id) == Optic (Fix Id) = True
  Optic (Fix (Compose a b)) == Optic (Fix (Compose c d)) =
    Optic a == Optic c && Optic b == Optic d
  _ == _ = False

instance Show Optic where
  showsPrec d (Optic (Fix (Field name))) =
    showParen (d > appPrec) $ showString "#" . showsPrec (appPrec + 1) name
  showsPrec _ (Optic (Fix Each)) = showString "each"
  showsPrec _ (Optic (Fix Id)) = showString "id"
  showsPrec d (Optic (Fix (Compose a b))) =
    showParen (d > composePrec)
      $ showsPrec prec (Optic a)
      . showString " . "
      . showsPrec prec (Optic b)
    where
      composePrec = 5
      prec = composePrec + 1

-- | Focus on a named field of a JSON object (affine traversal).
field :: Text -> Optic
field = Optic . Fix . Field

-- | Traverse all elements of a JSON array, or all values of an object.
-- Composed with another optic, it distributes that optic over each element:
-- @#users.each.#name@ focuses on the @name@ field of each array element.
each :: Optic
each = Optic (Fix Each)

-- | The identity optic: focuses on the whole value.
-- @id@ is the unit of optic composition: @compose id o = o@ and @compose o id = o@.
id :: Optic
id = Optic (Fix Id)

-- | Compose two optics sequentially.
-- @compose l r@ first focuses where @l@ points, then within each target,
-- focuses where @r@ points.
compose :: Optic -> Optic -> Optic
compose (Optic a) (Optic b) = Optic (Fix (Compose a b))
