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
import GHC.Float hiding (properFractionFloatInteger)
import GHC.Float.RealFracMethods hiding(properFractionFloatInteger, ceilingFloatInteger, floorFloatInteger, floorDoubleInteger)
import qualified GHC.Types
import System.Mem
import Debug.Trace (trace)
import Control.Exception (assert)
import Numeric (showHex)
import GHC.IntWord64
import GHC.Integer
import GHC.Base


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

{-
unIO :: GHC.Types.IO a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO (GHC.Types.IO m) = m
-}

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


mainWordAdd :: IO ()
mainWordAdd = do
    let x = 1 :: Word
    let y = 1 :: Word
    let (W# xp) = x
    let (W# yp) = y
    let r = case plusWord2# xp yp of
             (# 0##, _ #) -> False
             (# 1##, _ #) -> True
    putStrLn . show$ r

-- stolen from numrun012.hs
mainRIntDouble :: IO ()
mainRIntDouble = do
   print (int2Double (2^31))

log2 x = ceiling log_x
    where log_x :: Double
          -- log_x = logBase 2 (fromIntegral (max 1 x))
          log_x = 100 -- ((max 1 x))

properFractionFloatInteger :: Float -> IO (Integer, Float)
properFractionFloatInteger v@(F# x) =
    case decodeFloat_Int# x of
      (# m, e #)
        | isTrue# (e <# 0#) ->
          case negateInt# e of
            s | isTrue# (s ># 23#) -> return (0, v)
              | isTrue# (m <#  0#) ->
                case negateInt# (negateInt# m `uncheckedIShiftRA#` s) of
                  k -> return (smallInteger k,
                                case m -# (k `uncheckedIShiftL#` s) of
                                  r -> F# (encodeFloatInteger (smallInteger r) e))

              | otherwise           -> do
                case m `uncheckedIShiftRL#` s of
                  k -> do
                      let k' = smallInteger k
                      let l' = case m -# (k `uncheckedIShiftL#` s) of
                                      r -> F# (encodeFloatInteger (smallInteger r) e)
                      return (k', l')
        | otherwise -> return (shiftLInteger (smallInteger m) e, F# 0.0#)

ceilingFloatInteger :: Float -> IO Integer
ceilingFloatInteger (F# x) = do
    f <- floorFloatInteger (F# (negateFloat# x))
    return $ negateInteger f

{-# INLINE floorFloatInteger #-}
floorFloatInteger :: Float -> IO Integer
floorFloatInteger (F# x) = do
    case decodeFloat_Int# x of
      (# m, e #)
        | isTrue# (e <# 0#) ->
          case negateInt# e of
            s | isTrue# (s ># 23#) -> return $ if isTrue# (m <# 0#) then (-1) else 0
              | otherwise          -> return $ smallInteger (m `uncheckedIShiftRA#` s)
        | otherwise -> return $ shiftLInteger (smallInteger m) e


{-# INLINE floorDoubleInteger #-}
floorDoubleInteger :: Double -> IO Integer
floorDoubleInteger (D# x) =
    case decodeDoubleInteger x of
      (# m, e #)
        | isTrue# (e <# 0#) ->
          case negateInt# e of
            s | isTrue# (s ># 52#) -> return $ if m < 0 then (-1) else 0
              | otherwise          ->
                case integerToInt m of
                  n -> do
                      putStrLn $ "** in floorDoubleInteger **"
                      putStrLn $ "\tm:" <> show m <> "|" <> " e:" <> show (I# e) <> " |s: " <> show (I# s)
                      putStrLn $ "\tn:" <> show (I# n)
                      return $ smallInteger (n `uncheckedIShiftRA64#` s)

mainLog :: IO ()
mainLog = do
 -- let vals = [1, 2, 17, 259, 1000, 10000,
 --             2^30 + 9000, 2^31 - 1, 2^31 + 1,
 --             2^32 - 1, 2^32 + 1]
 let n = 100 :: Double
 putStrLn $ "decodeFloat: " <> show (decodeFloat  n)
 let m = negate n

 case m of
   (D# m') -> case (decodeDoubleInteger m') of
                    (# i, j #) -> putStrLn $  "decodeDoubleInteger: " <> show  (i, I#  j)
 putStrLn $ show $ "negate " <> show n <> ":" <> show m
 fl <- floorDoubleInteger (m)
 putStrLn $ "floor . negate: " <> show fl
 putStrLn $ show (ceilingDoubleInteger n)
 -- putStrLn (show (map log2 vals))
 -- let n = negate $ logBase 2 (fromIntegral 17)
 -- (int, decimal) <- properFractionFloatInteger n
 -- putStrLn $ "decode: " <> show (decodeFloat n)
 -- m <- floorFloatInteger n
 -- putStrLn $ "floorFloatInteger(" <> show  n <> ")" <> ":" <> show m

 -- putStrLn $ show n <> " decoded: " <> show (decodeFloat n)
 -- putStrLn $ show n <> " encoded: " <> show (encodeFloat m e)
 -- putStrLn $ "int: " <> show int <> " |decimal: " <> show decimal
 -- putStrLn . show $ (int + 1)

 -- let vals = [1, 2, 17, 259, 1000, 10000,
 --         2^30 + 9000, 2^31 - 1, 2^31 + 1,
 --         2^32 - 1, 2^32 + 1]
 -- putStrLn . show . (map log2) $ vals

mainDebugPrintDouble :: IO ()
mainDebugPrintDouble = do
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

main = mainLog
