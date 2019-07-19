{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -ddump-to-file -ddump-stg -ddump-cmm-raw -ddump-asm #-}

import Control.DeepSeq
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import GHC.Generics
import System.Mem
import Debug.Trace (trace)

fib :: Int -> Int
fib n = go 0 1 0
  where
    go !acc0 acc1 i
      | i == n = acc0
      | otherwise = go acc1 (acc0 + acc1) (i + 1)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

facts :: [Int]
facts = scanl (*) 1 [1 ..]

factMap :: Int -> IM.IntMap Int
factMap n = IM.fromList $ take n $ zip [0 ..] facts

sumFacts :: Int -> Int
sumFacts n =
  fst $
  flip execState (0, 0) $
  fix $ \w -> do
    (tot, i) <- get
    put (tot + facts !! i, i + 1)
    when (i < n) w

data BinTree
  = Tip
  | Branch !BinTree
           !BinTree
  deriving (Eq, Ord, Generic)

instance NFData BinTree

genBinTree :: Int -> BinTree
genBinTree 0 = Tip
genBinTree n = Branch t t
  where
    t = genBinTree (n - 1)

sizeofBinTree :: BinTree -> Int
sizeofBinTree Tip = 1
sizeofBinTree (Branch x y) = 1 + sizeofBinTree x + sizeofBinTree y

foreign import ccall safe "print_i64" print_i64 :: Int -> IO ()
foreign import ccall unsafe "assert_eq_i64" assert_eq_i64 :: Int -> Int -> IO ()

foreign import ccall unsafe "print_f64" print_f64 :: Double -> IO ()

main :: IO ()
main = do

  performGC

  putStrLn $ "xx"
  -- Test that assert_eq works
  -- assert_eq_i64 10 10

  -- print_i64 $ fib 10
  -- assert_eq_i64 (fib 10) 55

  -- print_i64 $ fact 5
  -- assert_eq_i64 (fact 5) 120

  -- print_f64 $ cos 0.5
  -- print_f64 $ 2 ** 3

  -- let sizeof3Tree = sizeofBinTree $ force $ genBinTree 3
  -- print_i64 $ sizeof3Tree
  -- -- 2^4 - 1
  -- assert_eq_i64 sizeof3Tree 15

  -- let sizeof5Tree = sizeofBinTree $ force $ genBinTree 5
  -- print_i64 $ sizeof5Tree
  -- -- 2^6 - 1
  -- assert_eq_i64 sizeof5Tree 63

  -- print_i64 $ facts !! 5
  -- assert_eq_i64 (facts !! 5) 120

  -- let factmapAt5 = factMap 10 IM.! 5
  -- print_i64 $ factmapAt5
  -- assert_eq_i64 factmapAt5 120

  -- -- 0! + 1! + 2! + 3! + 4! + 5!
  -- print_i64 $ sumFacts 5
  -- assert_eq_i64 (sumFacts 5) (154)

  -- performGC
