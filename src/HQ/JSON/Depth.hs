{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Shared nesting depth: the number of enclosing containers, tracking
-- 'length' of a container stack without traversing it. Used by the
-- encoder ('EncoderState') and the text-level skipper ("HQ.JSON.Skip")
-- alike; constructed once at 'initialDepth', pushed with 'deeper',
-- popped with 'shallower'.
module HQ.JSON.Depth
  ( NestDepth (..),
    initialDepth,
    deeper,
    shallower,
  )
where

import Relude

-- | Nesting depth: the number of enclosing containers.
-- Constructed once at 'initialDepth'; pushed with 'deeper', popped
-- with 'shallower'.
newtype NestDepth = NestDepth Int deriving (Eq, Ord, Show)

-- | The depth outside all containers.
initialDepth :: NestDepth
initialDepth = NestDepth 0

-- | Descend into a container.
deeper :: NestDepth -> NestDepth
deeper (NestDepth n) = NestDepth (n + 1)

-- | Ascend out of a container.
shallower :: NestDepth -> NestDepth
shallower (NestDepth n) = NestDepth (n - 1)
