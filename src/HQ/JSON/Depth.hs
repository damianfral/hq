{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Nesting depth: the number of enclosing containers.
module HQ.JSON.Depth
  ( NestDepth (..),
    initialDepth,
    deeper,
    shallower,
  )
where

import Relude

newtype NestDepth = NestDepth Int deriving (Eq, Ord, Show)

initialDepth :: NestDepth
initialDepth = NestDepth 0

deeper :: NestDepth -> NestDepth
deeper (NestDepth n) = NestDepth (n + 1)

shallower :: NestDepth -> NestDepth
shallower (NestDepth n) = NestDepth (n - 1)
