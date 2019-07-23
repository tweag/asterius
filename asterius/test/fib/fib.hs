{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -ddump-to-file -ddump-stg -ddump-cmm-raw -ddump-asm #-}

import Control.DeepSeq
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import GHC.Generics
-- import GHC.Float
import System.Mem
import Data.Char(intToDigit)
import Debug.Trace (trace)
import GHC.Arr

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


data FFFormat = FFExponent | FFFixed | FFGeneric

showFloat :: (RealFloat a) => a -> ShowS
showFloat x  =  showString (formatRealFloat FFGeneric Nothing x)

-- This is just a compatibility stub, as the "alt" argument formerly
-- didn't exist.
formatRealFloat :: (RealFloat a) => FFFormat -> Maybe Int -> a -> String
formatRealFloat fmt decs x = formatRealFloatAlt fmt decs False x

formatRealFloatAlt :: (RealFloat a) => FFFormat -> Maybe Int -> Bool -> a
                 -> String
formatRealFloatAlt fmt decs alt x
   | isNaN x                   = "NaN"
   | isInfinite x              = if x < 0 then "-Infinity" else "Infinity"
   | x < 0 || isNegativeZero x = '-':doFmt fmt (floatToDigits (toInteger base) (-x))
   | otherwise                 = doFmt fmt (floatToDigits (toInteger base) x)
 where
  base = 10

  doFmt format (is, e) =
    let ds = map intToDigit is in
    case format of
     FFGeneric ->
      doFmt (if e < 0 || e > 7 then FFExponent else FFFixed)
            (is,e)
     FFExponent ->
      case decs of
       Nothing ->
        let show_e' = show (e-1) in
        case ds of
          "0"     -> "0.0e0"
          [d]     -> d : ".0e" ++ show_e'
          (d:ds') -> d : '.' : ds' ++ "e" ++ show_e'
          []      -> errorWithoutStackTrace "formatRealFloat/doFmt/FFExponent: []"
       Just d | d <= 0 ->
        -- handle this case specifically since we need to omit the
        -- decimal point as well (#15115).
        -- Note that this handles negative precisions as well for consistency
        -- (see #15509).
        case is of
          [0] -> "0e0"
          _ ->
           let
             (ei,is') = roundTo base 1 is
             n:_ = map intToDigit (if ei > 0 then init is' else is')
           in n : 'e' : show (e-1+ei)
       Just dec ->
        let dec' = max dec 1 in
        case is of
         [0] -> '0' :'.' : take dec' (repeat '0') ++ "e0"
         _ ->
          let
           (ei,is') = roundTo base (dec'+1) is
           (d:ds') = map intToDigit (if ei > 0 then init is' else is')
          in
          d:'.':ds' ++ 'e':show (e-1+ei)
     FFFixed ->
      let
       mk0 ls = case ls of { "" -> "0" ; _ -> ls}
      in
      case decs of
       Nothing
          | e <= 0    -> "0." ++ replicate (-e) '0' ++ ds
          | otherwise ->
             let
                f 0 s    rs  = mk0 (reverse s) ++ '.':mk0 rs
                f n s    ""  = f (n-1) ('0':s) ""
                f n s (r:rs) = f (n-1) (r:s) rs
             in
                f e "" ds
       Just dec ->
        let dec' = max dec 0 in
        if e >= 0 then
         let
          (ei,is') = roundTo base (dec' + e) is
          (ls,rs)  = splitAt (e+ei) (map intToDigit is')
         in
         mk0 ls ++ (if null rs && not alt then "" else '.':rs)
        else
         let
          (ei,is') = roundTo base dec' (replicate (-e) 0 ++ is)
          d:ds' = map intToDigit (if ei > 0 then is' else 0:is')
         in
         d : (if null ds' && not alt then "" else '.':ds')

minExpt, maxExpt :: Int
minExpt = 0
maxExpt = 1100

maxExpt10 :: Int
maxExpt10 = 324

expts10 :: Array Int Integer
expts10 = array (minExpt,maxExpt10) [(n,10^n) | n <- [minExpt .. maxExpt10]]

expts :: Array Int Integer
expts = array (minExpt,maxExpt) [(n,2^n) | n <- [minExpt .. maxExpt]]


expt :: Integer -> Int -> Integer
expt base n =
    if base == 2 && n >= minExpt && n <= maxExpt then
        expts!n
    else
        if base == 10 && n <= maxExpt10 then
            expts10!n
        else
            base^n

roundTo :: Int -> Int -> [Int] -> (Int,[Int])
roundTo base d is =
  case f d True is of
    x@(0,_) -> x
    (1,xs)  -> (1, 1:xs)
    _       -> errorWithoutStackTrace "roundTo: bad Value"
 where
  b2 = base `quot` 2

  f n _ []     = (0, replicate n 0)
  f 0 e (x:xs) | x == b2 && e && all (== 0) xs = (0, [])   -- Round to even when at exactly half the base
               | otherwise = (if x >= b2 then 1 else 0, [])
  f n _ (i:xs)
     | i' == base = (1,0:ds)
     | otherwise  = (0,i':ds)
      where
       (c,ds) = f (n-1) (even i) xs
       i'     = c + i

floatToDigits :: (RealFloat a) => Integer -> a -> ([Int], Int)
floatToDigits _ 0 = ([0], 0)
floatToDigits base x =
 let
  (f0, e0) = decodeFloat x
  (minExp0, _) = floatRange x
  p = floatDigits x
  b = floatRadix x
  minExp = minExp0 - p -- the real minimum exponent
  -- Haskell requires that f be adjusted so denormalized numbers
  -- will have an impossibly low exponent.  Adjust for this.
  (f, e) =
   let n = minExp - e0 in
   if n > 0 then (f0 `quot` (expt b n), e0+n) else (f0, e0)
  (r, s, mUp, mDn) =
   if e >= 0 then
    let be = expt b e in
    if f == expt b (p-1) then
      (f*be*b*2, 2*b, be*b, be)     -- according to Burger and Dybvig
    else
      (f*be*2, 2, be, be)
   else
    if e > minExp && f == expt b (p-1) then
      (f*b*2, expt b (-e+1)*2, b, 1)
    else
      (f*2, expt b (-e)*2, 1, 1)
  k :: Int
  k =
   let
    k0 :: Int
    k0 =
     if b == 2 && base == 10 then
        -- logBase 10 2 is very slightly larger than 8651/28738
        -- (about 5.3558e-10), so if log x >= 0, the approximation
        -- k1 is too small, hence we add one and need one fixup step less.
        -- If log x < 0, the approximation errs rather on the high side.
        -- That is usually more than compensated for by ignoring the
        -- fractional part of logBase 2 x, but when x is a power of 1/2
        -- or slightly larger and the exponent is a multiple of the
        -- denominator of the rational approximation to logBase 10 2,
        -- k1 is larger than logBase 10 x. If k1 > 1 + logBase 10 x,
        -- we get a leading zero-digit we don't want.
        -- With the approximation 3/10, this happened for
        -- 0.5^1030, 0.5^1040, ..., 0.5^1070 and values close above.
        -- The approximation 8651/28738 guarantees k1 < 1 + logBase 10 x
        -- for IEEE-ish floating point types with exponent fields
        -- <= 17 bits and mantissae of several thousand bits, earlier
        -- convergents to logBase 10 2 would fail for long double.
        -- Using quot instead of div is a little faster and requires
        -- fewer fixup steps for negative lx.
        let lx = p - 1 + e0
            k1 = (lx * 8651) `quot` 28738
        in if lx >= 0 then k1 + 1 else k1
     else
        -- f :: Integer, log :: Float -> Float,
        --               ceiling :: Float -> Int
        ceiling ((log (fromInteger (f+1) :: Float) +
                 fromIntegral e * log (fromInteger b)) /
                   log (fromInteger base))
--WAS:            fromInt e * log (fromInteger b))

    fixup n =
      if n >= 0 then
        if r + mUp <= expt base n * s then n else fixup (n+1)
      else
        if expt base (-n) * (r + mUp) <= s then n else fixup (n+1)
   in
   fixup k0

  gen ds rn sN mUpN mDnN =
   let
    (dn, rn') = (rn * base) `quotRem` sN
    mUpN' = mUpN * base
    mDnN' = mDnN * base
   in
   case (rn' < mDnN', rn' + mUpN' > sN) of
    (True,  False) -> dn : ds
    (False, True)  -> dn+1 : ds
    (True,  True)  -> if rn' * 2 < sN then dn : ds else dn+1 : ds
    (False, False) -> gen (dn:ds) rn' sN mUpN' mDnN'

  rds =
   if k >= 0 then
      gen [] r (s * expt base k) mUp mDn
   else
     let bk = expt base (-k) in
     gen [] (r * bk) s (mUp * bk) (mDn * bk)
 in
 (map fromIntegral (reverse rds), k)


main :: IO ()
main = do
  let dbl = read "1.2" :: Double
  putStrLn $ showFloat dbl ""

mainold :: IO ()
mainold = do

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
