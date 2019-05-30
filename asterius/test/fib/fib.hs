{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -ddump-to-file -ddump-stg -ddump-cmm-raw -ddump-asm -fforce-recomp -ddump-simpl #-}

import Control.DeepSeq
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import Foreign
import GHC.Generics
import GHC.Exts
import GHC.Stack
import GHC.Integer
import qualified GHC.Types
import System.Mem
import Debug.Trace (trace)
import Control.Exception (assert)
import Numeric (showHex)

{-
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
-}

unI# :: Int -> Int#
unI# (I# x) = x

unIO :: GHC.Types.IO a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO (GHC.Types.IO m) = m

reinterpretCast :: (Storable a, Storable b) => a -> b
reinterpretCast a =
  case runRW#
         (\s0 ->
            case newPinnedByteArray# (unI# (sizeOf a)) s0 of
                (# s1, mba #) ->
                    case unsafeFreezeByteArray# mba s1 of
                      (# s2, ba #) ->
                        case byteArrayContents# ba of
                          addr ->
                            case unIO (poke (Ptr addr) a) s2 of
                              (# s3, _ #) -> unIO (peek (Ptr addr)) s3) of
                                (# _, r #) -> r

-- | Formats a 64 bit number as 16 digits hex.
hex16 :: Word64 -> String
hex16 i = let hex = showHex i ""
         in "0x" <> replicate (16 - length hex) '0' <> hex

one :: Double
one = 1


debugPrintDouble :: Double -> IO ()
debugPrintDouble d = do
     let (m, e) = decodeFloat d
     putStrLn $ "double: " <> show d
     putStrLn $ "\t" <> "mantissa: " <> show m
     putStrLn $ "\t" <> "exponent: " <> show e <> "|" <> (hex16 . reinterpretCast $ e)
     putStrLn $ "\t" <> "is infinite: " <> show (isInfinite d)


double2hex :: Double -> String
double2hex = hex16 . reinterpretCast
main :: IO ()
main = do
  let d = -0.0 :: Double
  let f = -0.0 :: Float

  debugPrintDouble 1
  debugPrintDouble (encodeFloat 1  2047)
  debugPrintDouble (encodeFloat 0xf000000000000 2047)
  debugPrintDouble $ encodeFloat 1 2048
  debugPrintDouble $ encodeFloat 1  2047               -- signalling NaN
  debugPrintDouble $  0/(0::Double)

  {-
  putStrLn $ "one * one: " <> show (one * one)
  putStrLn $ "d: " <> show d <> " | is neg 0: " <> show (isNegativeZero d)
  putStrLn $ "f: " <> show d <> " | is neg 0: " <> show (isNegativeZero f)
  putStrLn $ "d bytes: " <> double2hex d
  assert (isNegativeZero d == True) (pure ())

  let denorms = [4.9406564558412465e-324,
                 2.2250738585072014e-308,
                 0,
                 1,
                (one * one)] :: [Double]

  forM_ denorms $ \d -> do
      putStrLn $ show d <> " | decode: " <> show (decodeFloat d) <>  " | hex: " <> double2hex d <> " | is denorm: " <> show (isDenormalized d)
  -}

  {-
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
  -}
