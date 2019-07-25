{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -Wall -ddump-to-file -ddump-stg -ddump-cmm-raw -ddump-asm #-}

-- #include <iostream>
-- #include <float.h>
-- int main() {
--     std::cout << DBL_MANT_DIG << "\n";
-- }

#define DBL_MIN_EXP (-1021)
#define DBL_MAX_EXP 1024
#define DBL_MANT_DIG 53

import Control.DeepSeq
import Data.Ratio
import qualified Text.Read.Lex as L
import qualified Data.IntMap.Strict as IM
import Text.ParserCombinators.ReadPrec
import GHC.Generics
import GHC.Unicode
import Data.Semigroup ((<>))
-- import GHC.Float
import System.Mem
import Data.Char(intToDigit)
import Debug.Trace (trace)
import GHC.Arr
import Data.Bits
import Text.ParserCombinators.ReadPrec
import GHC.Real
import GHC.Word
import GHC.Arr
import GHC.Num
import GHC.Base
import GHC.Integer
import GHC.Types
-- import GHC.Float hiding(showFloat, expt, expts10, minExpt, maxExpt, maxExpt10, rationalToDouble, fromRat''_, floatToDigits, )
import GHC.Float.RealFracMethods
import GHC.Float.ConversionUtils
import GHC.Integer.Logarithms ( integerLogBase# )
import GHC.Integer.Logarithms.Internals

-- https://github.com/ghc/ghc/blob/f737033329817335bc01ab16a385b4b5ec5b3b5d/libraries/base/GHC/Float.hs

paren :: ReadPrec a -> ReadPrec a
-- ^ @(paren p)@ parses \"(P0)\"
--      where @p@ parses \"P0\" in precedence context zero
paren p = skipSpacesThenP (paren' p)

paren' :: ReadPrec a -> ReadPrec a
paren' p = expectCharP '(' $ reset p >>= \x ->
              skipSpacesThenP (expectCharP ')' (pure x))


expectCharP :: Char -> ReadPrec a -> ReadPrec a
expectCharP c a = do
  q <- get
  if q == c
    then a
    else pfail
{-# INLINE expectCharP #-}

parens :: ReadPrec a -> ReadPrec a
-- ^ @(parens p)@ parses \"P\", \"(P0)\", \"((P0))\", etc,
--      where @p@ parses \"P\"  in the current precedence context
--          and parses \"P0\" in precedence context zero
parens p = optional
  where
    optional = skipSpacesThenP (p +++ mandatory)
    mandatory = paren' optional

lexP :: ReadPrec L.Lexeme
-- ^ Parse a single lexeme
lexP = lift L.lex



skipSpacesThenP :: ReadPrec a -> ReadPrec a
skipSpacesThenP m =
  do s <- look
     skip s
 where
   skip (c:s) | isSpace c = get *> skip s
   skip _ = m




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



data FFFormat = FFExponent | FFFixed | FFGeneric

showFloat :: (RealFloat a) => a -> ShowS
showFloat x  =  showString (formatRealFloat Nothing x)

formatRealFloat :: (RealFloat a) => Maybe Int -> a
                 -> String
formatRealFloat decs x
   | isNaN x                   = "NaN"
   | isInfinite x              = if x < 0 then "-Infinity" else "Infinity"
   | x < 0 || isNegativeZero x = '-':doFmt (floatToDigits (toInteger base) (-x))
   | otherwise                 = doFmt (floatToDigits (toInteger base) x)
 where
  base = 10

  doFmt (is, e) =
    let ds = map intToDigit is
    in let
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
         mk0 ls ++ (if null rs then "" else '.':rs)
        else
         let
          (ei,is') = roundTo base dec' (replicate (-e) 0 ++ is)
          d:ds' = map intToDigit (if ei > 0 then is' else 0:is')
         in
         d : (if null ds' then "" else '.':ds')

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
  -- (f0, e0) = trace ("x:" <> show x <> "| f0: " <> show f0 <> " | e0: " <> show e0 ) $ decodeFloat x
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



readNumber :: Num a => (L.Lexeme -> ReadPrec a) -> ReadPrec a
readNumber convert =
  parens
  ( do x <- lexP
       case x of
         L.Symbol "-" -> do y <- lexP
                            n <- convert y
                            return (negate n)

         _   -> convert x
  )


convertFrac :: forall a . RealFloat a => L.Lexeme -> ReadPrec a
convertFrac (L.Number n) = let resRange = floatRange (undefined :: a)
                               rangedRational = L.numberToRangedRational resRange n
                           in (trace $ "L.number n: " <> show n <> " |rangedRational: " <> show rangedRational) $
                                 case rangedRational of
                                    Nothing -> error $ "number not in range"
                                    Just rat -> return $ fromRational rat



readNumber' :: (RealFloat a, Num a) => ReadPrec a
readNumber' = do
  x <- lexP
  convertFrac x

rationalToDouble :: Integer -> Integer -> Double
{-# NOINLINE [1] rationalToDouble #-}
rationalToDouble n 0
    | n == 0        = 0/0
    | n < 0         = (-1)/0
    | otherwise     = 1/0
rationalToDouble n d
    | n == 0        = encodeFloat 0 0
    | n < 0         = -(fromRat'' minEx mantDigs (-n) d)
    | otherwise     = fromRat'' minEx mantDigs n d
      where
        minEx       = DBL_MIN_EXP
        mantDigs    = DBL_MANT_DIG

fromRat'' minEx mantDigs n d = let out = fromRat''_ minEx mantDigs n d
 in trace ("out fromRat'': " <> show out) $ out

fromRat''_ :: RealFloat a => Int -> Int -> Integer -> Integer -> a
-- Invariant: n and d strictly positive
fromRat''_ minEx@(I# me#) mantDigs@(I# md#) n d =
  trace ("fromRat''_ : n: " <> show n <> "|d: " <> show d) $
    case integerLog2IsPowerOf2# d of
      (# ld#, pw# #)
        | isTrue# (pw# ==# 0#) -> trace ("ERRORXXXXERRRORXXXERRORXXX: " <> show (box2 (integerLog2IsPowerOf2# d))) $
          case integerLog2# n of
            ln# | isTrue# (ln# >=# (ld# +# me# -# 1#)) ->
                  -- this means n/d >= 2^(minEx-1), i.e. we are guaranteed to get
                  -- a normalised number, round to mantDigs bits
                  if isTrue# (ln# <# md#)
                    then encodeFloat n (I# (negateInt# ld#))
                    else let n'  = n `shiftR` (I# (ln# +# 1# -# md#))
                             n'' = case roundingMode# n (ln# -# md#) of
                                    0# -> n'
                                    2# -> n' + 1
                                    _  -> case fromInteger n' .&. (1 :: Int) of
                                            0 -> n'
                                            _ -> n' + 1
                         in encodeFloat n'' (I# (ln# -# ld# +# 1# -# md#))
                | otherwise ->
                  -- n/d < 2^(minEx-1), a denorm or rounded to 2^(minEx-1)
                  -- the exponent for encoding is always minEx-mantDigs
                  -- so we must shift right by (minEx-mantDigs) - (-ld)
                  case ld# +# (me# -# md#) of
                    ld'# | isTrue# (ld'# <=# 0#) -> -- we would shift left, so we don't shift
                           encodeFloat n (I# ((me# -# md#) -# ld'#))
                         | isTrue# (ld'# <=# ln#) ->
                           let n' = n `shiftR` (I# ld'#)
                           in case roundingMode# n (ld'# -# 1#) of
                                0# -> encodeFloat n' (minEx - mantDigs)
                                1# -> if fromInteger n' .&. (1 :: Int) == 0
                                        then encodeFloat n' (minEx-mantDigs)
                                        else encodeFloat (n' + 1) (minEx-mantDigs)
                                _  -> encodeFloat (n' + 1) (minEx-mantDigs)
                         | isTrue# (ld'# ># (ln# +# 1#)) -> encodeFloat 0 0 -- result of shift < 0.5
                         | otherwise ->  -- first bit of n shifted to 0.5 place
                           case integerLog2IsPowerOf2# n of
                            (# _, 0# #) -> encodeFloat 0 0  -- round to even
                            (# _, _ #)  -> encodeFloat 1 (minEx - mantDigs)
        | otherwise -> trace ("OTHERWISE: " <> show (box2 (integerLog2IsPowerOf2# d))) $
          let ln = I# (integerLog2# n)
              ld = I# ld#
              -- 2^(ln-ld-1) < n/d < 2^(ln-ld+1)
              p0 = max minEx (ln - ld)
              (n', d')
                | p0 < mantDigs = (n `shiftL` (mantDigs - p0), d)
                | p0 == mantDigs = (n, d)
                | otherwise     = (n, d `shiftL` (p0 - mantDigs))
              -- if ln-ld < minEx, then n'/d' < 2^mantDigs, else
              -- 2^(mantDigs-1) < n'/d' < 2^(mantDigs+1) and we
              -- may need one scaling step
              scale p a b
                | (b `shiftL` mantDigs) <= a = (p+1, a, b `shiftL` 1)
                | otherwise = (p, a, b)
              (p', n'', d'') = scale (p0-mantDigs) n' d'
              -- n''/d'' < 2^mantDigs and p' == minEx-mantDigs or n''/d'' >= 2^(mantDigs-1)
              rdq = case n'' `quotRem` d'' of
                     (q,r) -> case compare (r `shiftL` 1) d'' of
                                LT -> q
                                EQ -> if fromInteger q .&. (1 :: Int) == 0
                                        then q else q+1
                                GT -> q+1
          in  encodeFloat rdq p'


--
box2 :: (# Int#, Int# #) -> (Int, Int)
box2 (# a, b #) = (I# a, I# b)

main :: IO ()
main = do
  let mantDigs = DBL_MANT_DIG
  -- putStrLn $ "5 log 2: " <> show (box2 (integerLog2IsPowerOf2# 5))
  let n = 6
  let d = 5
  let (# ld#, pw# #) =  integerLog2IsPowerOf2# d
  let ln = I# (integerLog2# n)
  let ld = I# ld#
  let p0 = max DBL_MIN_EXP (ln - ld)
  let (n', d')
        | p0 < mantDigs = (n `shiftL` (mantDigs - p0), d)
        | p0 == mantDigs = (n, d)
        | otherwise     = (n, d `shiftL` (p0 - mantDigs))
  let scale p a b
        | (b `shiftL` mantDigs) <= a = (p+1, a, b `shiftL` 1)
        | otherwise = (p, a, b)
  let (p', n'', d'') = scale (p0-mantDigs) n' d'
  let rdq = case n'' `quotRem` d'' of
               (q,r) -> case compare (r `shiftL` 1) d'' of
                          LT -> q
                          EQ -> if fromInteger q .&. (1 :: Int) == 0
                                  then q else q+1
                          GT -> q+1
  putStrLn $ "-----"
  putStrLn $ "(ld, pw): " <> show (I# ld#, I# pw#)
  putStrLn $ "ln: " <> show ln
  putStrLn $ "ld: " <> show ld
  putStrLn $ "p0: " <> show p0
  putStrLn $ "n', d': " <> show (n', d')
  putStrLn $ "p', n'', d'': " <> show (p', n'', d'')
  putStrLn $ "rdq: " <> show rdq
  putStrLn $ "-----"


  putStrLn $ "6 % 5 as float: " <> show (fromRat'' DBL_MIN_EXP DBL_MANT_DIG 6 5 :: Double)

  -- let [(dbl :: Double, _)] = readPrec_to_S (lexP >>= convertFrac) 0 "1.2"
  -- let [(flt :: Float, _)] = readPrec_to_S (lexP >>= convertFrac) 0 "1.2"

  -- putStrLn $ showFloat flt ""
  -- putStrLn $ showFloat dbl ""

  putStrLn $ showFloat 1.2 ""
  putStrLn $ showFloat 1.2 ""

mainold :: IO ()
mainold = do

  performGC

  putStrLn $ trace "trace message" ""
