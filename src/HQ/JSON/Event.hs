{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.JSON.Event where

import Data.Scientific (Scientific)
import Relude hiding (Compose, id, many, some, state)

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------

data JSONEvent
  = JSONBeginObject
  | JSONEndObject
  | JSONBeginArray
  | JSONEndArray
  | JSONObjectKey Text
  | JSONNull
  | JSONBool Bool
  | JSONNumber Scientific
  | JSONString Text
  deriving (Eq, Show)
