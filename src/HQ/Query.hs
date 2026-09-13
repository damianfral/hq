{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Query where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.Aeson as JS
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Lazy as Map
import Data.Scientific (Scientific)
import Data.Vector (Vector)
import HQ.Optic
import Relude hiding (Compose, many, some)

data Value
  = Null
  | Bool Bool
  | Number Scientific
  | String Text
  | Array (Vector Value)
  | Object (Map Text Value)
  deriving (Eq, Ord, Show)

instance FromJSON Value where
  parseJSON JS.Null = pure Null
  parseJSON (JS.Bool b) = pure $ Bool b
  parseJSON (JS.Number n) = pure $ Number n
  parseJSON (JS.String s) = pure $ String s
  parseJSON (JS.Array xs) = Array <$> traverse parseJSON xs
  parseJSON (JS.Object obj) = Object <$> traverse parseJSON obj'
    where
      obj' = Map.fromList [(Key.toText k, v) | (k, v) <- KM.toList obj]

instance ToJSON Value where
  toJSON Null = JS.Null
  toJSON (Bool b) = JS.Bool b
  toJSON (Number n) = JS.Number n
  toJSON (String s) = JS.String s
  toJSON (Array xs) = JS.Array (fmap toJSON xs)
  toJSON (Object obj) =
    JS.Object $ KM.fromList $ bimap Key.fromText toJSON <$> Map.toList obj

--------------------------------------------------------------------------------

data Query
  = Fold Optic
  | Preview Optic
  | Set Optic Value
  | Delete Optic
  deriving (Show, Eq)
