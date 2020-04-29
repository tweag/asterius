{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Asterius.Internals.Parallel
  ( parallelFor_,
    parallelFoldMap,
  )
where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.IORef

-- | Given the worker thread pool capacity @c@, @parallelFor_ c xs f@ maps @f@
-- on @xs@ in parallel on the global thread pool. The results are ignored. If
-- @c = 1@ then it is equivalent to @mapM_ fn xs@ (but not necessarily in that
-- order), thus avoiding threading overhead.
parallelFor_ :: Int -> [a] -> (a -> IO r) -> IO ()
parallelFor_ n xs fn
  | n >= 2 = do
    input <- newIORef xs
    mvars <- replicateM n newEmptyMVar
    let getNextElem = atomicModifyIORef' input $ \case
          [] -> ([], Nothing)
          (y : ys) -> (ys, Just y)
        loop mvar = getNextElem >>= \case
          Nothing -> putMVar mvar ()
          Just y -> fn y >> loop mvar
    forM_ ([0 ..] `zip` mvars) $ \(i, mvar) ->
      forkOn i (loop mvar)
    forM_ mvars takeMVar
  -- If there are not enough resources, fall back to the sequential version.
  | otherwise = mapM_ fn xs

-- | Given the worker thread pool capacity @c@, @parallelFoldMap c xs f@ maps @f@
-- on @xs@ in parallel on the global thread pool, and concatenates the results.
-- If @c = 1@ then it is equivalent to @mconcat <$> mapM fn xs@, thus avoiding
-- threading overhead. NOTE: this function does not only assume associativity
-- for @<>@ (as promised by the @Monoid@ instance), but also symmetry.
parallelFoldMap :: Monoid r => Int -> [a] -> (a -> IO r) -> IO r
parallelFoldMap n xs fn
  | n >= 2 = do
    input <- newIORef xs
    mvars <- replicateM n newEmptyMVar
    let getNextElem = atomicModifyIORef' input $ \case
          [] -> ([], Nothing)
          (y : ys) -> (ys, Just y)
        loop mvar !acc = getNextElem >>= \case
          Nothing -> putMVar mvar acc
          Just y -> do
            res <- fn y
            loop mvar (acc <> res)
    forM_ ([0 ..] `zip` mvars) $ \(i, mvar) ->
      forkOn i (loop mvar mempty)
    mconcat <$> forM mvars takeMVar
  -- If there are not enough resources, fall back to the sequential version.
  | otherwise = mconcat <$> mapM fn xs
