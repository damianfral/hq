{-# LANGUAGE NoImplicitPrelude #-}

module HQ where

import Data.Fix (Fix (..), foldFix)
import qualified Data.Map.Lazy as Map
import qualified Data.Vector as V
import HQ.Optic
import HQ.Query
import Relude hiding (Compose, id, many, some)

-- | Fold an optic path over a JSON value, collecting all focused targets.
--
-- This is the \"read\" side of optics. The algebra interprets each
-- optic constructor as a function @Value -> [Value]@, and composition
-- is Kleisli composition (concatMap) in the list monad.
runFold :: Optic -> Value -> [Value]
runFold (Optic optic) = foldFix algebra optic
  where
    algebra :: OpticF (Value -> [Value]) -> Value -> [Value]
    algebra (Field name) (Object obj) = maybe [] pure $ Map.lookup name obj
    algebra (Field _) _ = []
    algebra Each (Array v) = toList v
    algebra Each (Object obj) = Map.elems obj
    algebra Each _ = []
    algebra Id val = [val]
    algebra (Compose left right) value = concatMap right $ left value

-- | Modify all values focused by an optic path with the given function.
--
-- This is the \"write\" side of optics. Each constructor specifies
-- how to propagate the modification into the JSON structure.
-- Composition is functor composition: @over (l . r) f = over l (over r f)@.
runOver :: Fix OpticF -> (Value -> Value) -> Value -> Value
runOver (Fix (Field name)) f (Object m) = Object $ case Map.lookup name m of
  Just v -> Map.insert name (f v) m
  Nothing -> m
runOver (Fix (Field _)) _ val = val
runOver (Fix Each) f (Array xs) = Array $ f <$> xs
runOver (Fix Each) f (Object obj) = Object $ f <$> obj
runOver (Fix Each) _ val = val
runOver (Fix Id) f val = f val
runOver (Fix (Compose l r)) f val = runOver l (runOver r f) val

-- | Replace all values focused by an optic with the given replacement value.
runSet :: Optic -> Value -> Value -> Value
runSet (Optic o) newVal = runOver o (const newVal)

-- | Remove all values focused by an optic.
--
-- Deletion semantics:
--
-- * @Field name@ on an Object: remove the named key
-- * @Each@ on an Array: empty the array
-- * @Each@ on an Object: clear all values
-- * @Every@: remove the focused value from each array element
-- * @Id@: cannot delete the whole document (returns unchanged)
-- * @Compose l r@: delete through @l@ by running @delete r@ on each target
runDelete :: Optic -> Value -> Value
runDelete (Optic (Fix (Field name))) (Object m) = Object $ Map.delete name m
runDelete (Optic (Fix Each)) (Array _) = Array V.empty
runDelete (Optic (Fix Each)) (Object _) = Object Map.empty
runDelete (Optic (Fix Id)) _ = Null
runDelete (Optic (Fix (Compose l r))) val = runOver l (runDelete (Optic r)) val
runDelete _ val = val

--------------------------------------------------------------------------------

-- | The result of executing a query.
data Result
  = -- | A query that focuses on at most one value.
    Single (Maybe Value)
  | -- | A query that focuses on zero or more values.
    Multi [Value]
  deriving (Show, Eq)

-- | Execute a query against a JSON value.
executeQuery :: Query -> Value -> Result
executeQuery (Preview optic) val = Single $ listToMaybe $ runFold optic val
executeQuery (Fold optic) val = Multi $ runFold optic val
executeQuery (Set optic newVal) val = Single $ Just $ runSet optic newVal val
executeQuery (Delete optic) val = Single $ Just $ runDelete optic val
