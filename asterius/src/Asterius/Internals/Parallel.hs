module Asterius.Internals.Parallel
  ( parallelFor
    -- parallelFor_,
  )
where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad

-- -- | Given the worker thread pool capacity @c@, @parallelFor c xs f@ maps @f@
-- -- on @xs@ in parallel on the global thread pool (the results are ignored). If
-- -- @c = 1@ then it is equivalent to @mconcat <$> mapM fn xs@, thus avoiding
-- -- threading overhead.
-- {-# INLINE parallelFor_ #-}
-- parallelFor_ :: Int -> [a] -> (a -> IO r) -> IO ()
-- parallelFor_ n xs fn
--   | n >= 2 = error "TODO"
--   -- If there are not enough resources, fall back to the sequential version.
--   | otherwise = mapM_ fn xs

-- | Given the worker thread pool capacity @c@, @parallelFor c xs f@ maps @f@
-- on @xs@ in parallel on the global thread pool, and concatenates the results.
-- If @c = 1@ then it is equivalent to @mconcat <$> mapM fn xs@, thus avoiding
-- threading overhead.
{-# INLINE parallelFor #-}
parallelFor :: Monoid r => Int -> [a] -> (a -> IO r) -> IO r
parallelFor n xs fn
  | n >= 2 = do
    let chunks = mkChunks 0 xs
    mvars <- replicateM (length chunks) newEmptyMVar
    forM_ (chunks `zip` mvars) $ \((i, ys), mvar) -> do
      forkOn i $ do
        cont <- mconcat <$> mapM fn ys
        putMVar mvar cont
    mconcat <$> forM mvars takeMVar
  -- If there are not enough resources, fall back to the sequential version.
  | otherwise = mconcat <$> mapM fn xs
  where
    mkChunks i es
      | i < n - 1 =
        let (ys, yss) = splitAt chunkSize es
         in (i, ys) : mkChunks (i + 1) yss
      | otherwise = [(i, es)] -- leftovers
    chunkSize =
      let l = length xs
       in if mod l n == 0 then div l n else div l n + 1
