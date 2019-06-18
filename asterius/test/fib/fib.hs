{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
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
  -- print $ "double: "
  mapM_ print [invDeviation @Double sin asin (-1.3)]

  -- print  ((\(x, y) -> (x, invDeviation @Double sin asin y)) <$> (zip [1,2..] [-1.5, -1.4 .. 1.5]))
  -- print  ((\x -> invDeviation @Double sin asin x) <$> [-1.5, -1.4 .. 1.5])
  -- print [(invDeviation @Double sin asin <$> [-1.5, -1.4 .. 1.5])]
  -- print [(invDeviation @Float sin asin <$> [-1.5, -1.4 .. 1.5])]
  -- print $ invDeviation @Double cos acos <$> [0, 0.1 .. 3]
  -- let x = (-1.4 :: Double)
  -- print $ (asin x :: Double)
  -- print $ (asin (sin  x) :: Double)
  -- print $ (asin (sin  x) / x :: Double)
  -- print $ (asin (sin  x) / x - 1 :: Double)
  -- print $ (((fromIntegral $ (round $ ((asin (sin  x) / x) - 1) * 2^36)) / (2^36 :: Double)) :: Double)
  -- print $ invDeviation @Double sin asin x
  -- print $ "float: "
  -- mapM_ print $ invDeviation @Float sin asin <$> [-1.5, -1.4 .. 1.5]

invDeviation :: KnownNumDeviation a
          => (a -> a) -- ^ Some numerical function @f@.
          -> (a -> a) -- ^ Inverse @g = f⁻¹@ of that function.
          -> a        -- ^ Value @x@ which to compare with @g (f x)@.
          -> Double   -- ^ Relative discrepancy between original/expected
                      --   value and actual function result.
invDeviation f g 0 = rmNumericDeviation $ (g (f 0) + 1) - 1
invDeviation f g x = rmNumericDeviation $ (g (f x) / x) - 1

-- | We need to round results to some sensible precision,
--   because floating-point arithmetic generally makes
--   it impossible to /exactly/ invert functions.
--   What precision this is depends on the type. The bounds
--   here are rather generous; the functions should usually
--   perform substantially better than that.
class (Floating a, Eq a) => KnownNumDeviation a where
  rmNumericDeviation :: a -> Double

instance KnownNumDeviation Double where
  rmNumericDeviation x = fromIntegral (round $ x * 2^36) / 2^36

instance KnownNumDeviation Float where
  rmNumericDeviation x = fromIntegral (round $ x * 2^16) / 2^16


main_ :: IO ()
main_ = do

  performGC

  putStrLn $ trace "trace message" ""

  -- Test that assert_eq works
  assert_eq_i64 10 10

  print_i64 $ fib 10
  assert_eq_i64 (fib 10) 55

  print_i64 $ fact 5
  assert_eq_i64 (fact 5) 120

  print_f64 $ cos 0.5
  print_f64 $ 2 ** 3

  let sizeof3Tree = sizeofBinTree $ force $ genBinTree 3
  print_i64 $ sizeof3Tree
  -- 2^4 - 1
  assert_eq_i64 sizeof3Tree 15

  let sizeof5Tree = sizeofBinTree $ force $ genBinTree 5
  print_i64 $ sizeof5Tree
  -- 2^6 - 1
  assert_eq_i64 sizeof5Tree 63

  print_i64 $ facts !! 5
  assert_eq_i64 (facts !! 5) 120

  let factmapAt5 = factMap 10 IM.! 5
  print_i64 $ factmapAt5
  assert_eq_i64 factmapAt5 120

  -- 0! + 1! + 2! + 3! + 4! + 5!
  print_i64 $ sumFacts 5
  assert_eq_i64 (sumFacts 5) (154)

  performGC
