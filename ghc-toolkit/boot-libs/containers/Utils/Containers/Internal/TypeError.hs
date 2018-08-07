{-# LANGUAGE DataKinds, FlexibleInstances, FlexibleContexts, UndecidableInstances,
     KindSignatures, TypeFamilies, CPP #-}

#if !defined(TESTING)
# if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE Safe #-}
# else
{-# LANGUAGE Trustworthy #-}
#endif
#endif

-- | Unsatisfiable constraints for functions being removed.

module Utils.Containers.Internal.TypeError where
import GHC.TypeLits

-- | The constraint @Whoops s@ is unsatisfiable for every 'Symbol' @s@.
-- Under GHC 8.0 and above, trying to use a function with a @Whoops s@
-- constraint will lead to a pretty type error explaining how to fix
-- the problem. Under earlier GHC versions, it will produce an extremely
-- ugly type error within which the desired message is buried.
--
-- ==== Example
--
-- @
-- oldFunction :: Whoops "oldFunction is gone now. Use newFunction."
--             => Int -> IntMap a -> IntMap a
-- @
class Whoops (a :: Symbol)

#if __GLASGOW_HASKELL__ >= 800
instance TypeError ('Text a) => Whoops a
#endif

-- Why don't we just use
--
-- type Whoops a = TypeError ('Text a) ?
--
-- When GHC sees the type signature of oldFunction, it will see that it
-- has an unsatisfiable constraint and reject it out of hand.
--
-- It seems possible to hack around that with a type family:
--
-- type family Whoops a where
--   Whoops a = TypeError ('Text a)
--
-- but I don't really trust that to work reliably. What we actually
-- do is pretty much guaranteed to work. Despite the fact that there
-- is a totally polymorphic instance in scope, GHC will refrain from
-- reducing the constraint because it knows someone could (theoretically)
-- define an overlapping instance of Whoops. It doesn't commit to
-- the polymorphic one until it has to, at the call site.
