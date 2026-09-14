{-# LANGUAGE NoImplicitPrelude #-}

module HQ where

import Data.Fix (Fix (..), foldFix)
import qualified Data.Map.Lazy as Map
import qualified Data.Vector as V
import HQ.Optic
import HQ.Query
import HQ.Value (Value (..))
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
    algebra PrismString val@(String _) = [val]
    algebra PrismString _ = []
    algebra PrismNumber val@(Number _) = [val]
    algebra PrismNumber _ = []
    algebra PrismBool val@(Bool _) = [val]
    algebra PrismBool _ = []
    algebra PrismNull val@Null = [val]
    algebra PrismNull _ = []
    algebra PrismArray val@(Array _) = [val]
    algebra PrismArray _ = []
    algebra PrismObject val@(Object _) = [val]
    algebra PrismObject _ = []
    algebra PrismJust Null = []
    algebra PrismJust val = [val]
    algebra Prism1 (Array v) | not (null v) = [V.head v]
    algebra Prism1 _ = []
    algebra Prism2 (Array v) | V.length v >= 2 = [v V.! 1]
    algebra Prism2 _ = []

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
runOver (Fix PrismString) f (String s) = f (String s)
runOver (Fix PrismString) _ val = val
runOver (Fix PrismNumber) f (Number n) = f (Number n)
runOver (Fix PrismNumber) _ val = val
runOver (Fix PrismBool) f (Bool b) = f (Bool b)
runOver (Fix PrismBool) _ val = val
runOver (Fix PrismNull) f Null = f Null
runOver (Fix PrismNull) _ val = val
runOver (Fix PrismArray) f (Array xs) = f (Array xs)
runOver (Fix PrismArray) _ val = val
runOver (Fix PrismObject) f (Object m) = f (Object m)
runOver (Fix PrismObject) _ val = val
runOver (Fix PrismJust) _ Null = Null
runOver (Fix PrismJust) f val = f val
runOver (Fix Prism1) f (Array xs)
  | not (null xs) = Array $ V.cons (f (V.head xs)) (V.tail xs)
runOver (Fix Prism1) _ val = val
runOver (Fix Prism2) f (Array xs)
  | V.length xs >= 2 = Array $ V.cons (V.head xs) (V.cons (f (xs V.! 1)) (V.drop 2 xs))
runOver (Fix Prism2) _ val = val

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
runDelete (Optic (Fix PrismString)) (String _) = Null
runDelete (Optic (Fix PrismNumber)) (Number _) = Null
runDelete (Optic (Fix PrismBool)) (Bool _) = Null
runDelete (Optic (Fix PrismNull)) Null = Null
runDelete (Optic (Fix PrismArray)) (Array _) = Null
runDelete (Optic (Fix PrismObject)) (Object _) = Null
runDelete (Optic (Fix PrismJust)) Null = Null
runDelete (Optic (Fix PrismJust)) val = val
runDelete (Optic (Fix Prism1)) (Array xs)
  | not (null xs) = Array $ V.cons Null (V.tail xs)
runDelete (Optic (Fix Prism2)) (Array xs)
  | V.length xs >= 2 = Array $ V.cons (V.head xs) (V.cons Null (V.drop 2 xs))
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
