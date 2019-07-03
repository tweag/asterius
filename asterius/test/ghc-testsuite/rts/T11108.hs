{-# LANGUAGE RecursiveDo, LambdaCase, BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics (Generic, Generic1)
import Control.Monad.Fix
import Data.IORef
import System.Mem.Weak
import System.Mem
import Control.Exception(evaluate)
import Control.DeepSeq
import Control.Monad
import Data.Foldable

data Pull = Pull
  { weakSelf  :: Weak Pull
  , compute :: Weak Pull -> IO Int
  } deriving(Generic)


makePull :: (Weak Pull -> IO Int) -> IO Pull
makePull f = do
  rec
    -- This seems to be the culprit, changing the order makes the weakRef get gc'ed
    -- In this configuration it crashes
    let !foo = Pull weak f
    weak <- mkWeakPtr foo Nothing

  return foo


invalidate :: Pull -> IO ()
invalidate p = do
  return ()


pull :: Weak Pull -> Pull -> IO Int
pull weak p = do
  pull' p

pull' :: Pull -> IO Int
pull' p = do
      r <- compute p (weakSelf p)
      return r

add :: Pull -> Int -> IO (Pull)
add p n = makePull (\w -> (+n) <$> pull w p)

main = do
  h <- newIORef 0

  performGC

  source <- makePull (const $ readIORef h)
  p <- foldM add source (take 10000 (repeat 1))   -- 100 is not enough for crash
  print =<< pull' p
  performGC



