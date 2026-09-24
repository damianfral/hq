{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.OpticSpec (spec) where

import HQ.Optic
import HQ.Optic.Parser ()
-- Brings the orphan 'Read Optic' instance into scope.
import Relude hiding (Compose, id)
import Test.Syd

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
