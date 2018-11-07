{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Asterius.Internals.Codensity
  ( Codensity
  , liftCodensity
  , runCodensity
  , lowerCodensity
  ) where

import Control.Monad.Cont
import qualified Control.Monad.Fail as Fail

newtype Codensity m a = Codensity
  { unCodensity :: forall r. ContT r m a
  }

instance Functor (Codensity m) where
  {-# INLINE fmap #-}
  fmap f (Codensity m) = Codensity (fmap f m)

instance Applicative (Codensity m) where
  {-# INLINE pure #-}
  pure a = Codensity (pure a)
  {-# INLINE (<*>) #-}
  Codensity f <*> Codensity a = Codensity (f <*> a)

instance Monad (Codensity m) where
  {-# INLINE (>>=) #-}
  Codensity m >>= f = Codensity (m >>= unCodensity . f)

instance Fail.MonadFail m => Fail.MonadFail (Codensity m) where
  {-# INLINE fail #-}
  fail e = Codensity (Fail.fail e)

instance MonadIO m => MonadIO (Codensity m) where
  {-# INLINE liftIO #-}
  liftIO m = Codensity (liftIO m)

instance MonadTrans Codensity where
  {-# INLINE lift #-}
  lift m = Codensity (lift m)

{-# INLINE liftCodensity #-}
liftCodensity :: (forall r. (a -> m r) -> m r) -> Codensity m a
liftCodensity c = Codensity (ContT c)

{-# INLINE runCodensity #-}
runCodensity :: Codensity m a -> (a -> m r) -> m r
runCodensity (Codensity m) = runContT m

{-# INLINE lowerCodensity #-}
lowerCodensity :: Applicative f => Codensity f a -> f a
lowerCodensity = flip runCodensity pure
