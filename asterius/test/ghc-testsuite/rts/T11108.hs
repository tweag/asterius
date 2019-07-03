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

pull' :: Pull -> IO Int
pull' p = do
      r <- compute p (weakSelf p)
      return r

incr :: Pull -> IO Pull 
incr p = makePull (\w -> (+1) <$> pull' p)

repeatM :: Monad m => Int -> (a -> m a) -> a -> m a
repeatM 0 _ a = return a
repeatM n f a = do 
  a' <- f a
  repeatM (n - 1) f a'

main = do
  h <- newIORef 0
  source <- makePull (const $ readIORef h)
  p <- repeatM 10000 incr source
  print =<< pull' p
  performGC



