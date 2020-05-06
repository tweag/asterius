{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :  Asterius.Internals.Parallel
-- Copyright   :  (c) 2018 EURL Tweag
-- License     :  All rights reserved (see LICENCE file in the distribution).
--
-- Simple parallel combinators. Since we need to control our dependency
-- surface, our current approach to parallelism is very simple: given the
-- worker thread pool capacity @c@ and the list of tasks to be performed,
-- 'parallelFoldMap' pins exactly @c@ threads on each of the capabilities, lets
-- them consume the input concurrently, and gathers the results using their
-- 'Monoid' instance. Notice that this behavior is deterministic only if '<>'
-- is also symmetric (not only associative), but that is sufficient for our
-- usecases.
--
-- To avoid needless threading overhead, if @c = 1@ them we fall back to the
-- sequential implementation.
module Asterius.Internals.Parallel
  ( parallelRnf,
    parallelFoldMap,
  )
where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.IORef
import System.IO.Unsafe

-- | Given the worker thread pool capacity @c@, @parallelRnf c xs@ deeply
-- evaluates a list of objects in parallel on the global thread pool.
parallelRnf :: NFData a => Int -> [a] -> ()
parallelRnf n xs
  | n >= 2 = unsafePerformIO $ parallelFoldMap n xs (void . evaluate . force)
  | otherwise = rnf xs

-- | Given the worker thread pool capacity @c@, @parallelFoldMap c xs f@ maps @f@
-- on @xs@ in parallel on the global thread pool, and concatenates the results.
parallelFoldMap :: (NFData r, Monoid r) => Int -> [a] -> (a -> IO r) -> IO r
parallelFoldMap n xs fn
  | n >= 2 = do
    input <- newIORef xs
    mvars <- replicateM n newEmptyMVar
    let getNextElem = atomicModifyIORef' input $ \case
          [] -> ([], Nothing)
          (y : ys) -> (ys, Just y)
        loop mvar !acc = getNextElem >>= \case -- was (force -> !acc)
          Nothing -> putMVar mvar acc
          Just y -> do
            (force -> !res) <- fn y -- was: res <- fn y
            loop mvar (acc <> res)
    forM_ ([0 ..] `zip` mvars) $ \(i, mvar) ->
      forkOn i (loop mvar mempty)
    mconcat <$> forM mvars takeMVar
  | otherwise = mconcat <$> mapM fn xs
