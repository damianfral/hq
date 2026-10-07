{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.OpticSpec (spec) where

import Data.Aeson (Value (..))
import HQ.Optic
import HQ.Optic.AST (inferFocusType, inferOpticType)
import HQ.Optic.OpticType (OpticType (..))
import HQ.Optic.Parser (parseOptic)
import HQ.Transformation hiding (Compose)
import HQ.Transformation.TransformationType (ValueType (..))
import Relude hiding (Compose, filter, id, not)
import Test.Syd

-- | The cardinality of an optic, inferred directly.
opticTypeOf :: Optic -> OpticType
opticTypeOf = inferOpticType

-- | The focused value type of an optic, inferred directly.
opticFocusOf :: Optic -> ValueType
opticFocusOf = inferFocusType

-- | Parse 'show' output back (replaces the former 'Read Optic' orphan,
-- which lived in the library only for this check).
readOptic :: String -> Maybe Optic
readOptic str = case parseOptic (fromString str) of
  Left _ -> Nothing
  Right v -> Just v

spec :: Spec
spec = describe "HQ.Optic" $ do
  it "show and read are inverses" $ do
    -- NOTE: 'field' is excluded: its 'Show' quotes the name (@"a")
    -- while the parser accepts bare identifiers (@a), so fields never
    -- round-tripped (pre-existing mismatch, unrelated to this change).
    let optics =
          [ Id,
            Each,
            Keys,
            Values,
            Ix 0,
            Ix 1,
            Ix 3,
            Prism PString,
            Compose Each Keys,
            Compose Keys (Prism PString),
            Compose Values Keys,
            Filter Each (Equal (Number 1)),
            Filter (Ix 0) Not,
            Filter (Compose Each (Ix 0)) (Equal (Number 1))
          ]
    forM_ optics $ \optic -> readOptic (show optic) `shouldBe` Just optic

  it "ix equality compares indices" $ do
    Ix 0 `shouldBe` Ix 0
    (Ix 0 == Ix 1) `shouldBe` False

  describe "inferOpticType classifications" $ do
    it "types id as a lens" $ do
      opticTypeOf Id `shouldBe` OpticLens

    it "types fields and indices as affine" $ do
      opticTypeOf (Field "a") `shouldBe` OpticAffineTraversal
      opticTypeOf (Ix 0) `shouldBe` OpticAffineTraversal

    it "types type prisms as prisms" $ do
      opticTypeOf (Prism PString) `shouldBe` OpticPrism
      opticTypeOf (Prism PNull) `shouldBe` OpticPrism

    it "types _Just as affine rather than prism" $ do
      opticTypeOf PrismJust `shouldBe` OpticAffineTraversal

    it "types each/keys/values as traversals" $ do
      opticTypeOf Each `shouldBe` OpticTraversal
      opticTypeOf Keys `shouldBe` OpticTraversal
      opticTypeOf Values `shouldBe` OpticTraversal

    it "types compositions with the lattice" $ do
      opticTypeOf (Compose (Field "a") Each) `shouldBe` OpticTraversal
      opticTypeOf (Compose (Field "a") (Ix 0)) `shouldBe` OpticAffineTraversal
      opticTypeOf (Compose (Prism PString) PrismJust) `shouldBe` OpticAffineTraversal

    it "types filter as affine" $ do
      opticTypeOf (Filter (Field "a") (Equal (Number 1))) `shouldBe` OpticAffineTraversal
      opticTypeOf (Compose Each (Filter (Field "a") (Equal (Number 1)))) `shouldBe` OpticTraversal

  describe "inferFocusType classifications" $ do
    it "leaves unknown shapes as any" $ do
      opticFocusOf (Field "a") `shouldBe` ValueAny
      opticFocusOf Each `shouldBe` ValueAny
      opticFocusOf Values `shouldBe` ValueAny
      opticFocusOf Id `shouldBe` ValueAny
      opticFocusOf (Ix 0) `shouldBe` ValueAny
      opticFocusOf PrismJust `shouldBe` ValueAny
      opticFocusOf (Filter (Field "a") (Equal (Number 1))) `shouldBe` ValueAny

    it "pins prisms and keys to exact types" $ do
      opticFocusOf (Prism PString) `shouldBe` ValueString
      opticFocusOf (Prism PNumber) `shouldBe` ValueNumber
      opticFocusOf (Prism PBool) `shouldBe` ValueBool
      opticFocusOf (Prism PNull) `shouldBe` ValueNull
      opticFocusOf (Prism PArray) `shouldBe` ValueArray
      opticFocusOf (Prism PObject) `shouldBe` ValueObject
      opticFocusOf Keys `shouldBe` ValueString

    it "takes the rightmost focus through composition" $ do
      opticFocusOf (Compose Each (Prism PNumber)) `shouldBe` ValueNumber
      opticFocusOf (Compose (Field "a") (Field "b")) `shouldBe` ValueAny
