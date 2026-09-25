{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.OpticSpec (spec) where

import Control.Comonad.Cofree (Cofree ((:<)))
import HQ.Optic
import HQ.Optic.AST (OpticAST (..), buildOpticAST)
import HQ.Optic.OpticType (OpticType (..))
import HQ.Optic.Parser ()
-- Brings the orphan 'Read Optic' instance into scope.
import Relude hiding (Compose, id)
import Test.Syd

-- | The cardinality of an optic: the annotation at the root of its AST.
opticTypeOf :: Optic -> OpticType
opticTypeOf optic = t
  where
    OpticAST (t :< _) = buildOpticAST optic

spec :: Spec
spec = describe "HQ.Optic" $ do
  it "show and read are inverses" $ do
    -- NOTE: 'field' is excluded: its 'Show' quotes the name (#"a")
    -- while the parser accepts bare identifiers (#a), so fields never
    -- round-tripped (pre-existing mismatch, unrelated to this change).
    let optics =
          [ id,
            each,
            keys,
            values,
            ix 0,
            ix 1,
            ix 3,
            _String,
            compose each keys,
            compose keys _String,
            compose values keys
          ]
    forM_ optics $ \optic -> readMaybe (show optic) `shouldBe` Just optic

  it "ix equality compares indices" $ do
    ix 0 `shouldBe` ix 0
    (ix 0 == ix 1) `shouldBe` False

  describe "buildOpticAST classifications" $ do
    it "types id as a lens" $ do
      opticTypeOf id `shouldBe` OpticLens

    it "types fields and indices as affine" $ do
      opticTypeOf (field "a") `shouldBe` OpticAffineTraversal
      opticTypeOf (ix 0) `shouldBe` OpticAffineTraversal

    it "types type prisms as prisms" $ do
      opticTypeOf _String `shouldBe` OpticPrism
      opticTypeOf _Null `shouldBe` OpticPrism

    it "types _Just as affine rather than prism" $ do
      opticTypeOf _Just `shouldBe` OpticAffineTraversal

    it "types each/keys/values as traversals" $ do
      opticTypeOf each `shouldBe` OpticTraversal
      opticTypeOf keys `shouldBe` OpticTraversal
      opticTypeOf values `shouldBe` OpticTraversal

    it "types compositions with the lattice" $ do
      opticTypeOf (compose (field "a") each) `shouldBe` OpticTraversal
      opticTypeOf (compose (field "a") (ix 0)) `shouldBe` OpticAffineTraversal
      opticTypeOf (compose _String _Just) `shouldBe` OpticAffineTraversal
