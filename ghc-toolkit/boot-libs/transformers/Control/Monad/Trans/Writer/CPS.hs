{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Writer.CPS
-- Copyright   :  (c) Daniel Mendler 2016,
--                (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- The strict 'WriterT' monad transformer, which adds collection of
-- outputs (such as a count or string output) to a given monad.
--
-- This monad transformer provides only limited access to the output
-- during the computation. For more general access, use
-- "Control.Monad.Trans.State" instead.
--
-- This version builds its output strictly and uses continuation-passing-style
-- to achieve constant space usage. This transformer can be used as a
-- drop-in replacement for "Control.Monad.Trans.Writer.Strict".
-----------------------------------------------------------------------------

module Control.Monad.Trans.Writer.CPS (
    -- * The Writer monad
    Writer,
    writer,
    runWriter,
    execWriter,
    mapWriter,
    -- * The WriterT monad transformer
    WriterT,
    writerT,
    runWriterT,
    execWriterT,
    mapWriterT,
    -- * Writer operations
    tell,
    listen,
    listens,
    pass,
    censor,
    -- * Lifting other operations
    liftCallCC,
    liftCatch,
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Signatures
import Data.Functor.Identity

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid
#endif

#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif

-- ---------------------------------------------------------------------------
-- | A writer monad parameterized by the type @w@ of output to accumulate.
--
-- The 'return' function produces the output 'mempty', while '>>='
-- combines the outputs of the subcomputations using 'mappend'.
type Writer w = WriterT w Identity

-- | Construct a writer computation from a (result, output) pair.
-- (The inverse of 'runWriter'.)
writer :: (Monoid w, Monad m) => (a, w) -> WriterT w m a
writer (a, w') = WriterT $ \ w ->
    let wt = w `mappend` w' in wt `seq` return (a, wt)
{-# INLINE writer #-}

-- | Unwrap a writer computation as a (result, output) pair.
-- (The inverse of 'writer'.)
runWriter :: (Monoid w) => Writer w a -> (a, w)
runWriter = runIdentity . runWriterT
{-# INLINE runWriter #-}

-- | Extract the output from a writer computation.
--
-- * @'execWriter' m = 'snd' ('runWriter' m)@
execWriter :: (Monoid w) => Writer w a -> w
execWriter = runIdentity . execWriterT
{-# INLINE execWriter #-}

-- | Map both the return value and output of a computation using
-- the given function.
--
-- * @'runWriter' ('mapWriter' f m) = f ('runWriter' m)@
mapWriter :: (Monoid w, Monoid w') =>
    ((a, w) -> (b, w')) -> Writer w a -> Writer w' b
mapWriter f = mapWriterT (Identity . f . runIdentity)
{-# INLINE mapWriter #-}

-- ---------------------------------------------------------------------------
-- | A writer monad parameterized by:
--
--   * @w@ - the output to accumulate.
--
--   * @m@ - The inner monad.
--
-- The 'return' function produces the output 'mempty', while '>>='
-- combines the outputs of the subcomputations using 'mappend'.

newtype WriterT w m a = WriterT { unWriterT :: w -> m (a, w) }

-- | Construct a writer computation from a (result, output) computation.
-- (The inverse of 'runWriterT'.)
writerT :: (Functor m, Monoid w) => m (a, w) -> WriterT w m a
writerT f = WriterT $ \ w ->
    (\ (a, w') -> let wt = w `mappend` w' in wt `seq` (a, wt)) <$> f
{-# INLINE writerT #-}

-- | Unwrap a writer computation.
-- (The inverse of 'writerT'.)
runWriterT :: (Monoid w) => WriterT w m a -> m (a, w)
runWriterT m = unWriterT m mempty
{-# INLINE runWriterT #-}

-- | Extract the output from a writer computation.
--
-- * @'execWriterT' m = 'liftM' 'snd' ('runWriterT' m)@
execWriterT :: (Monad m, Monoid w) => WriterT w m a -> m w
execWriterT m = do
    (_, w) <- runWriterT m
    return w
{-# INLINE execWriterT #-}

-- | Map both the return value and output of a computation using
-- the given function.
--
-- * @'runWriterT' ('mapWriterT' f m) = f ('runWriterT' m)@
mapWriterT :: (Monad n, Monoid w, Monoid w') =>
    (m (a, w) -> n (b, w')) -> WriterT w m a -> WriterT w' n b
mapWriterT f m = WriterT $ \ w -> do
    (a, w') <- f (runWriterT m)
    let wt = w `mappend` w'
    wt `seq` return (a, wt)
{-# INLINE mapWriterT #-}

instance (Functor m) => Functor (WriterT w m) where
    fmap f m = WriterT $ \ w -> (\ (a, w') -> (f a, w')) <$> unWriterT m w
    {-# INLINE fmap #-}

instance (Functor m, Monad m) => Applicative (WriterT w m) where
    pure a = WriterT $ \ w -> return (a, w)
    {-# INLINE pure #-}

    WriterT mf <*> WriterT mx = WriterT $ \ w -> do
        (f, w') <- mf w
        (x, w'') <- mx w'
        return (f x, w'')
    {-# INLINE (<*>) #-}

instance (Functor m, MonadPlus m) => Alternative (WriterT w m) where
    empty = WriterT $ const mzero
    {-# INLINE empty #-}

    WriterT m <|> WriterT n = WriterT $ \ w -> m w `mplus` n w
    {-# INLINE (<|>) #-}

instance (Monad m) => Monad (WriterT w m) where
#if !(MIN_VERSION_base(4,8,0))
    return a = WriterT $ \ w -> return (a, w)
    {-# INLINE return #-}
#endif

    m >>= k = WriterT $ \ w -> do
        (a, w') <- unWriterT m w
        unWriterT (k a) w'
    {-# INLINE (>>=) #-}

#if !(MIN_VERSION_base(4,13,0))
    fail msg = WriterT $ \ _ -> fail msg
    {-# INLINE fail #-}
#endif

#if MIN_VERSION_base(4,9,0)
instance (Fail.MonadFail m) => Fail.MonadFail (WriterT w m) where
    fail msg = WriterT $ \ _ -> Fail.fail msg
    {-# INLINE fail #-}
#endif

instance (Functor m, MonadPlus m) => MonadPlus (WriterT w m) where
    mzero = empty
    {-# INLINE mzero #-}
    mplus = (<|>)
    {-# INLINE mplus #-}

instance (MonadFix m) => MonadFix (WriterT w m) where
    mfix f = WriterT $ \ w -> mfix $ \ ~(a, _) -> unWriterT (f a) w
    {-# INLINE mfix #-}

instance MonadTrans (WriterT w) where
    lift m = WriterT $ \ w -> do
        a <- m
        return (a, w)
    {-# INLINE lift #-}

instance (MonadIO m) => MonadIO (WriterT w m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

-- | @'tell' w@ is an action that produces the output @w@.
tell :: (Monoid w, Monad m) => w -> WriterT w m ()
tell w = writer ((), w)
{-# INLINE tell #-}

-- | @'listen' m@ is an action that executes the action @m@ and adds its
-- output to the value of the computation.
--
-- * @'runWriterT' ('listen' m) = 'liftM' (\\ (a, w) -> ((a, w), w)) ('runWriterT' m)@
listen :: (Monoid w, Monad m) => WriterT w m a -> WriterT w m (a, w)
listen = listens id
{-# INLINE listen #-}

-- | @'listens' f m@ is an action that executes the action @m@ and adds
-- the result of applying @f@ to the output to the value of the computation.
--
-- * @'listens' f m = 'liftM' (id *** f) ('listen' m)@
--
-- * @'runWriterT' ('listens' f m) = 'liftM' (\\ (a, w) -> ((a, f w), w)) ('runWriterT' m)@
listens :: (Monoid w, Monad m) =>
    (w -> b) -> WriterT w m a -> WriterT w m (a, b)
listens f m = WriterT $ \ w -> do
    (a, w') <- runWriterT m
    let wt = w `mappend` w'
    wt `seq` return ((a, f w'), wt)
{-# INLINE listens #-}

-- | @'pass' m@ is an action that executes the action @m@, which returns
-- a value and a function, and returns the value, applying the function
-- to the output.
--
-- * @'runWriterT' ('pass' m) = 'liftM' (\\ ((a, f), w) -> (a, f w)) ('runWriterT' m)@
pass :: (Monoid w, Monoid w', Monad m) =>
    WriterT w m (a, w -> w') -> WriterT w' m a
pass m = WriterT $ \ w -> do
    ((a, f), w') <- runWriterT m
    let wt = w `mappend` f w'
    wt `seq` return (a, wt)
{-# INLINE pass #-}

-- | @'censor' f m@ is an action that executes the action @m@ and
-- applies the function @f@ to its output, leaving the return value
-- unchanged.
--
-- * @'censor' f m = 'pass' ('liftM' (\\ x -> (x,f)) m)@
--
-- * @'runWriterT' ('censor' f m) = 'liftM' (\\ (a, w) -> (a, f w)) ('runWriterT' m)@
censor :: (Monoid w, Monad m) => (w -> w) -> WriterT w m a -> WriterT w m a
censor f m = WriterT $ \ w -> do
    (a, w') <- runWriterT m
    let wt = w `mappend` f w'
    wt `seq` return (a, wt)
{-# INLINE censor #-}

-- | Uniform lifting of a @callCC@ operation to the new monad.
-- This version rolls back to the original state on entering the
-- continuation.
liftCallCC :: CallCC m (a, w) (b, w) -> CallCC (WriterT w m) a b
liftCallCC callCC f = WriterT $ \ w ->
    callCC $ \ c -> unWriterT (f (\ a -> WriterT $ \ _ -> c (a, w))) w
{-# INLINE liftCallCC #-}

-- | Lift a @catchE@ operation to the new monad.
liftCatch :: Catch e m (a, w) -> Catch e (WriterT w m) a
liftCatch catchE m h = WriterT $ \ w ->
    unWriterT m w `catchE` \ e -> unWriterT (h e) w
{-# INLINE liftCatch #-}
