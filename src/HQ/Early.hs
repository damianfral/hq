{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Type-safe early return without 'ExceptT':
-- https://burningwitness.github.io/blog/posts/against-effect-systems/
--
-- 'ExceptT' threads an 'Either' through every action, taxing every
-- bind even on the success path. Instead, failures throw a private
-- exception ('ReturningEarly') caught once at the edge ('runEarly').
-- The 'Early' token scopes the power to throw: only code holding it
-- can fail with @e@, tracked as a plain function argument.
module HQ.Early where

import Control.Exception (catch, throwIO)
import GHC.Show
import Relude

type role Early nominal

type Early :: Type -> Type
data Early e = Early

type role ReturningEarly nominal

type ReturningEarly :: Type -> Type
newtype ReturningEarly e = ReturningEarly e

instance (Typeable e) => Show (ReturningEarly e) where
  show = displayException

instance (Typeable e) => Exception (ReturningEarly e) where
  displayException (ReturningEarly _) = "Early return exception"

-- | Abort with @e@. Needs 'Early' in scope, so only code explicitly
-- given the capability can fail this way.
leave :: (Typeable e) => Early e -> e -> IO a
leave _ e = throwIO (ReturningEarly e)

-- | Run an action that may leave early, catching the failure at the edge.
runEarly :: (Typeable e) => (Early e -> IO a) -> IO (Either e a)
runEarly f = catch (Right <$> f Early) (\(ReturningEarly e) -> pure (Left e))

-- | Lift an 'Either' into 'IO', leaving early on failure.
orLeave :: (Typeable e) => Early e -> Either e a -> IO a
orLeave early r = case r of
  Left e -> leave early e
  Right v -> pure v
