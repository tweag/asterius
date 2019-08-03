{-# LANGUAGE MagicHash, NoImplicitPrelude, UnboxedTuples,
             UnliftedFFITypes #-}

-- Commentary of Integer library is located on the wiki:
-- http://ghc.haskell.org/trac/ghc/wiki/Commentary/Libraries/Integer
--
-- It gives an in-depth description of implementation details and
-- decisions.

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Integer.Type
-- Copyright   :  (c) Ian Lynagh 2007-2012
-- License     :  BSD3
--
-- Maintainer  :  igloo@earth.li
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- A simple definition of the 'Integer' type.
--
-----------------------------------------------------------------------------

module GHC.Integer.Type where

import Asterius.Magic
import GHC.Classes
import GHC.Magic
import GHC.Prim
import GHC.Types

data Integer = Integer Int#

mkInteger :: Bool -> [Int] -> Integer
mkInteger nonNegative is =
  runRW#
    (\s0 ->
       case unIO (js_newInteger nonNegative) s0 of
         (# s1, i0 #) ->
           let w [] sx = (# sx, () #)
               w (x:xs) sx =
                 case unIO (js_prependInteger i0 x) sx of
                   (# sy, _ #) -> w xs sy
            in case w is s1 of
                 (# s2, _ #) ->
                   case unIO (js_freezeInteger i0) s2 of
                     (# _, I# r #) -> Integer r)


-- NOTE: small integer and integers larger than Number.MAX_SAFE_INTEGER
-- 64 bit = 64 / 16 = 4
smallInteger :: Int# -> Integer
smallInteger i =
    let w = int2Word# i
        low32mask# = 4294967295##
        low = and# w low32mask#
        high = and# (uncheckedShiftRL# w 32#) low32mask#
    in Integer (js_smallInteger high low)

wordToInteger :: Word# -> Integer
wordToInteger w =
    let low32mask# = 4294967295##
        low = and# w low32mask#
        high = and# (uncheckedShiftRL# w 32#) low32mask#
    in Integer (js_wordToInteger high low)

integerToWord :: Integer -> Word#
integerToWord (Integer i) = or# (uncheckedShiftL# (js_integerToWord i 1#) 32#)  (js_integerToWord i 0#)

integerToInt :: Integer -> Int#
integerToInt i = word2Int# (integerToWord i)

plusInteger :: Integer -> Integer -> Integer
plusInteger (Integer i0) (Integer i1) = Integer (js_plusInteger i0 i1)

minusInteger :: Integer -> Integer -> Integer
minusInteger (Integer i0) (Integer i1) = Integer (js_minusInteger i0 i1)

timesInteger :: Integer -> Integer -> Integer
timesInteger (Integer i0) (Integer i1) = Integer (js_timesInteger i0 i1)

negateInteger :: Integer -> Integer
negateInteger (Integer i) = Integer (js_negateInteger i)

eqInteger :: Integer -> Integer -> Bool
eqInteger (Integer i0) (Integer i1) = js_eqInteger i0 i1

neqInteger :: Integer -> Integer -> Bool
neqInteger (Integer i0) (Integer i1) = js_neqInteger i0 i1

absInteger :: Integer -> Integer
absInteger (Integer i) = Integer (js_absInteger i)

signumInteger :: Integer -> Integer
signumInteger (Integer i) = Integer (js_signumInteger i)

leInteger :: Integer -> Integer -> Bool
leInteger (Integer i0) (Integer i1) = js_leInteger i0 i1

gtInteger :: Integer -> Integer -> Bool
gtInteger (Integer i0) (Integer i1) = js_gtInteger i0 i1

ltInteger :: Integer -> Integer -> Bool
ltInteger (Integer i0) (Integer i1) = js_ltInteger i0 i1

geInteger :: Integer -> Integer -> Bool
geInteger (Integer i0) (Integer i1) = js_geInteger i0 i1

compareInteger :: Integer -> Integer -> Ordering
compareInteger (Integer i0) (Integer i1) =
  if js_eqInteger i0 i1
    then EQ
    else if js_leInteger i0 i1
           then LT
           else GT

eqInteger# :: Integer -> Integer -> Int#
eqInteger# (Integer i0) (Integer i1) = unBool (js_eqInteger i0 i1)

neqInteger# :: Integer -> Integer -> Int#
neqInteger# (Integer i0) (Integer i1) = unBool (js_neqInteger i0 i1)

leInteger# :: Integer -> Integer -> Int#
leInteger# (Integer i0) (Integer i1) = unBool (js_leInteger i0 i1)

gtInteger# :: Integer -> Integer -> Int#
gtInteger# (Integer i0) (Integer i1) = unBool (js_gtInteger i0 i1)

ltInteger# :: Integer -> Integer -> Int#
ltInteger# (Integer i0) (Integer i1) = unBool (js_ltInteger i0 i1)

geInteger# :: Integer -> Integer -> Int#
geInteger# (Integer i0) (Integer i1) = unBool (js_geInteger i0 i1)

divModInteger :: Integer -> Integer -> (# Integer, Integer #)
n `divModInteger` d =
    case n `quotRemInteger` d of
        (# q, r #) ->
            if signumInteger r `eqInteger`
               negateInteger (signumInteger d)
            then (# q `minusInteger` oneInteger, r `plusInteger` d #)
            else (# q, r #)

divInteger :: Integer -> Integer -> Integer
n `divInteger` d = quotient
    where (# quotient, _ #) = n `divModInteger` d

modInteger :: Integer -> Integer -> Integer
n `modInteger` d = modulus
    where (# _, modulus #) = n `divModInteger` d

quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger (Integer i0) (Integer i1) =
  (# Integer (js_quotInteger i0 i1), Integer (js_remInteger i0 i1) #)

quotInteger :: Integer -> Integer -> Integer
quotInteger (Integer i0) (Integer i1) = Integer (js_quotInteger i0 i1)

remInteger :: Integer -> Integer -> Integer
remInteger (Integer i0) (Integer i1) = Integer (js_remInteger i0 i1)

encodeFloatInteger :: Integer -> Int# -> Float#
encodeFloatInteger (Integer m) n = js_encodeFloatInteger m n

decodeFloatInteger :: Float# -> (# Integer, Int# #)
decodeFloatInteger f =
  (# Integer (js_decodeFloatInteger_m f), js_decodeFloatInteger_n f #)

floatFromInteger :: Integer -> Float#
floatFromInteger (Integer i) = js_floatFromInteger i

encodeDoubleInteger :: Integer -> Int# -> Double#
encodeDoubleInteger (Integer m) n = js_encodeDoubleInteger m n

decodeDoubleInteger :: Double# -> (# Integer, Int# #)
decodeDoubleInteger d = (# Integer (js_decodeDoubleInteger_m d), js_decodeDoubleInteger_e d #)


doubleFromInteger :: Integer -> Double#
doubleFromInteger (Integer i) = js_doubleFromInteger i

gcdInteger :: Integer -> Integer -> Integer
gcdInteger (Integer i0) (Integer i1) = Integer (js_gcdInteger i0 i1)

lcmInteger :: Integer -> Integer -> Integer
lcmInteger (Integer i0) (Integer i1) = Integer (js_lcmInteger i0 i1)

andInteger :: Integer -> Integer -> Integer
andInteger (Integer i0) (Integer i1) = Integer (js_andInteger i0 i1)

orInteger :: Integer -> Integer -> Integer
orInteger (Integer i0) (Integer i1) = Integer (js_orInteger i0 i1)

xorInteger :: Integer -> Integer -> Integer
xorInteger (Integer i0) (Integer i1) = Integer (js_xorInteger i0 i1)

complementInteger :: Integer -> Integer
complementInteger (Integer i) = Integer (js_complementInteger i)

shiftLInteger :: Integer -> Int# -> Integer
shiftLInteger (Integer i0) i1 = Integer (js_shiftLInteger i0 i1)

shiftRInteger :: Integer -> Int# -> Integer
shiftRInteger (Integer i0) i1 = Integer (js_shiftRInteger i0 i1)

testBitInteger :: Integer -> Int# -> Bool
testBitInteger (Integer i0) i1 = js_testBitInteger i0 i1

popCountInteger :: Integer -> Int#
popCountInteger (Integer i) = js_popCountInteger i

bitInteger :: Int# -> Integer
bitInteger i = Integer (js_bitInteger i)

hashInteger :: Integer -> Int#
hashInteger (Integer i) = js_hashInteger i

oneInteger :: Integer
oneInteger = smallInteger 1#

powInteger :: Integer -> Integer -> Integer
powInteger (Integer i0) (Integer i1) = Integer (js_powInteger i0 i1)

instance Eq Integer where
  (==) = eqInteger
  (/=) = neqInteger

instance Ord Integer where
  (<=) = leInteger
  (>) = gtInteger
  (<) = ltInteger
  (>=) = geInteger
  compare = compareInteger

foreign import javascript "__asterius_jsffi.Integer.newInteger(${1})" js_newInteger :: Bool -> IO Int

foreign import javascript "__asterius_jsffi.Integer.prependInteger(${1},${2})" js_prependInteger :: Int -> Int -> IO ()

foreign import javascript "__asterius_jsffi.Integer.freezeInteger(${1})" js_freezeInteger :: Int -> IO Int

foreign import javascript "__asterius_jsffi.Integer.smallInteger(${1}, ${2})" js_smallInteger :: Word# -> Word# -> Int#

foreign import javascript "__asterius_jsffi.Integer.wordToInteger(${1}, ${2})" js_wordToInteger :: Word# -> Word# -> Int#

-- | Given integer and which 32-bit _piece_ of the word we want, return that piece.
foreign import javascript "__asterius_jsffi.Integer.integerToWord(${1}, ${2})" js_integerToWord :: Int# -> Int# -> Word#


foreign import javascript "__asterius_jsffi.Integer.plusInteger(${1},${2})" js_plusInteger :: Int# -> Int# -> Int#

foreign import javascript "__asterius_jsffi.Integer.minusInteger(${1},${2})" js_minusInteger :: Int# -> Int# -> Int#

foreign import javascript "__asterius_jsffi.Integer.timesInteger(${1},${2})" js_timesInteger :: Int# -> Int# -> Int#

foreign import javascript "__asterius_jsffi.Integer.negateInteger(${1})" js_negateInteger :: Int# -> Int#

foreign import javascript "__asterius_jsffi.Integer.eqInteger(${1},${2})" js_eqInteger :: Int# -> Int# -> Bool

foreign import javascript "__asterius_jsffi.Integer.neqInteger(${1},${2})" js_neqInteger :: Int# -> Int# -> Bool

foreign import javascript "__asterius_jsffi.Integer.absInteger(${1})" js_absInteger :: Int# -> Int#

foreign import javascript "__asterius_jsffi.Integer.signumInteger(${1})" js_signumInteger :: Int# -> Int#

foreign import javascript "__asterius_jsffi.Integer.leInteger(${1},${2})" js_leInteger :: Int# -> Int# -> Bool

foreign import javascript "__asterius_jsffi.Integer.gtInteger(${1},${2})" js_gtInteger :: Int# -> Int# -> Bool

foreign import javascript "__asterius_jsffi.Integer.ltInteger(${1},${2})" js_ltInteger :: Int# -> Int# -> Bool

foreign import javascript "__asterius_jsffi.Integer.geInteger(${1},${2})" js_geInteger :: Int# -> Int# -> Bool

foreign import javascript "__asterius_jsffi.Integer.quotInteger(${1},${2})" js_quotInteger :: Int# -> Int# -> Int#

foreign import javascript "__asterius_jsffi.Integer.remInteger(${1},${2})" js_remInteger :: Int# -> Int# -> Int#

foreign import javascript "__asterius_jsffi.Integer.encodeDoubleInteger(${1},${2})" js_encodeFloatInteger :: Int# -> Int# -> Float#

foreign import javascript "__asterius_jsffi.Integer.encode(__asterius_jsffi.FloatCBits.decodeDoubleInteger(${1})[0])" js_decodeFloatInteger_m :: Float# -> Int#

foreign import javascript "__asterius_jsffi.Integer.decodeDoubleInteger(${1})[1]" js_decodeFloatInteger_n :: Float# -> Int#

foreign import javascript "__asterius_jsffi.Integer.doubleFromInteger(${1})" js_floatFromInteger :: Int# -> Float#

foreign import javascript "__asterius_jsffi.Integer.encodeDoubleInteger(${1},${2})" js_encodeDoubleInteger :: Int# -> Int# -> Double#

foreign import javascript "__asterius_jsffi.Integer.encode(__asterius_jsffi.FloatCBits.decodeDoubleInteger(${1})[0])" js_decodeDoubleInteger_m :: Double# -> Int#

foreign import javascript "__asterius_jsffi.FloatCBits.decodeDoubleInteger(${1})[1]" js_decodeDoubleInteger_e :: Double# -> Int#

foreign import javascript "__asterius_jsffi.Integer.doubleFromInteger(${1})" js_doubleFromInteger :: Int# -> Double#

foreign import javascript "__asterius_jsffi.Integer.gcdInteger(${1},${2})" js_gcdInteger :: Int# -> Int# -> Int#

foreign import javascript "__asterius_jsffi.Integer.lcmInteger(${1},${2})" js_lcmInteger :: Int# -> Int# -> Int#

foreign import javascript "__asterius_jsffi.Integer.andInteger(${1},${2})" js_andInteger :: Int# -> Int# -> Int#

foreign import javascript "__asterius_jsffi.Integer.orInteger(${1},${2})" js_orInteger :: Int# -> Int# -> Int#

foreign import javascript "__asterius_jsffi.Integer.xorInteger(${1},${2})" js_xorInteger :: Int# -> Int# -> Int#

foreign import javascript "__asterius_jsffi.Integer.complementInteger(${1})" js_complementInteger :: Int# -> Int#

foreign import javascript "__asterius_jsffi.Integer.shiftLInteger(${1},${2})" js_shiftLInteger :: Int# -> Int# -> Int#

foreign import javascript "__asterius_jsffi.Integer.shiftRInteger(${1},${2})" js_shiftRInteger :: Int# -> Int# -> Int#

foreign import javascript "__asterius_jsffi.Integer.testBitInteger(${1},${2})" js_testBitInteger :: Int# -> Int# -> Bool

foreign import javascript "__asterius_jsffi.Integer.popCountInteger(${1})" js_popCountInteger :: Int# -> Int#

foreign import javascript "__asterius_jsffi.Integer.bitInteger(${1})" js_bitInteger :: Int# -> Int#

foreign import javascript "__asterius_jsffi.Integer.hashInteger(${1})" js_hashInteger :: Int# -> Int#

foreign import javascript "__asterius_jsffi.Integer.powInteger(${1},${2})" js_powInteger :: Int# -> Int# -> Int#
