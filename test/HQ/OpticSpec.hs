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
    let optics = [id, each]
    forM_ optics $ \optic -> readMaybe (show optic) `shouldBe` Just optic
