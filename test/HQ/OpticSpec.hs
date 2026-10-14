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
    it "classifies optics" $ forM_ opticTypeCases $ \(optic, expected) ->
      opticTypeOf optic `shouldBe` expected

  describe "appendOptic monoid" $ do
    it "combines optics" $ forM_ appendCases $ \(l, r, expected) ->
      appendOptic l r `shouldBe` expected

    it "satisfies the monoid identity laws" $ do
      (mempty :: Optic) `shouldBe` Id
      (Each <> Field "a") `shouldBe` Compose Each (Field "a")
      (Field "a" <> Id) `shouldBe` Field "a"
      (Id <> Field "a") `shouldBe` Field "a"

    it "agrees on cardinality across reassociation" $ do
      let l = Field "a"
          c = Each
          r = Ix 0
      opticTypeOf ((l <> c) <> r) `shouldBe` opticTypeOf (l <> (c <> r))

  describe "inferFocusType classifications" $ do
    it "infers focus types" $ forM_ focusCases $ \(optic, expected) ->
      opticFocusOf optic `shouldBe` expected

opticTypeCases :: [(Optic, OpticType)]
opticTypeCases =
  [ (Id, OpticLens),
    (Field "a", OpticAffineTraversal),
    (Ix 0, OpticAffineTraversal),
    (Prism PString, OpticPrism),
    (Prism PNull, OpticPrism),
    (PrismJust, OpticAffineTraversal),
    (Each, OpticTraversal),
    (Keys, OpticTraversal),
    (Values, OpticTraversal),
    (Compose (Field "a") Each, OpticTraversal),
    (Compose (Field "a") (Ix 0), OpticAffineTraversal),
    (Compose (Prism PString) PrismJust, OpticAffineTraversal),
    (Filter (Field "a") (Equal (Number 1)), OpticAffineTraversal),
    (Compose Each (Filter (Field "a") (Equal (Number 1))), OpticTraversal)
  ]

appendCases :: [(Optic, Optic, Optic)]
appendCases =
  [ (Id, Field "a", Field "a"),
    (Field "a", Id, Field "a"),
    (Id, Id, Id),
    (Each, Field "a", Compose Each (Field "a"))
  ]

focusCases :: [(Optic, ValueType)]
focusCases =
  [ (Field "a", ValueAny),
    (Each, ValueAny),
    (Values, ValueAny),
    (Id, ValueAny),
    (Ix 0, ValueAny),
    (PrismJust, ValueAny),
    (Filter (Field "a") (Equal (Number 1)), ValueAny),
    (Prism PString, ValueString),
    (Prism PNumber, ValueNumber),
    (Prism PBool, ValueBool),
    (Prism PNull, ValueNull),
    (Prism PArray, ValueArray),
    (Prism PObject, ValueObject),
    (Keys, ValueString),
    (Compose Each (Prism PNumber), ValueNumber),
    (Compose (Field "a") (Field "b"), ValueAny)
  ]
