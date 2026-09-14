{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.StreamSpec (spec) where

import Relude hiding (Compose, id)
import Test.Syd

spec :: Spec
spec =
  xdescribe "HQ.Stream"
    $ pending "Tests need to be rewritten to match current Stream API"
