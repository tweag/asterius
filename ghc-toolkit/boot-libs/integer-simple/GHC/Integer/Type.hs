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
import Asterius.Prim
import GHC.Classes
import GHC.Magic
import GHC.Prim
import GHC.Types

newtype Integer = Integer JSVal

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
                     (# _, r #) -> r)

-- NOTE: small integer and integers larger than Number.MAX_SAFE_INTEGER
-- 64 bit = 64 / 16 = 4
smallInteger :: Int# -> Integer
smallInteger i =
  let w = int2Word# i
      low32mask# = 4294967295##
      low = and# w low32mask#
      high = and# (uncheckedShiftRL# w 32#) low32mask#
   in js_smallInteger high low

wordToInteger :: Word# -> Integer
wordToInteger w =
  let low32mask# = 4294967295##
      low = and# w low32mask#
      high = and# (uncheckedShiftRL# w 32#) low32mask#
   in js_wordToInteger high low

integerToWord :: Integer -> Word#
integerToWord i =
  or# (uncheckedShiftL# (js_integerToWord i 1#) 32#) (js_integerToWord i 0#)

integerToInt :: Integer -> Int#
integerToInt i = word2Int# (integerToWord i)

compareInteger :: Integer -> Integer -> Ordering
compareInteger i0 i1 =
  if eqInteger i0 i1
    then EQ
    else if leInteger i0 i1
           then LT
           else GT

eqInteger# :: Integer -> Integer -> Int#
eqInteger# i0 i1 = unBool (eqInteger i0 i1)

neqInteger# :: Integer -> Integer -> Int#
neqInteger# i0 i1 = unBool (neqInteger i0 i1)

leInteger# :: Integer -> Integer -> Int#
leInteger# i0 i1 = unBool (leInteger i0 i1)

gtInteger# :: Integer -> Integer -> Int#
gtInteger# i0 i1 = unBool (gtInteger i0 i1)

ltInteger# :: Integer -> Integer -> Int#
ltInteger# i0 i1 = unBool (ltInteger i0 i1)

geInteger# :: Integer -> Integer -> Int#
geInteger# i0 i1 = unBool (geInteger i0 i1)

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
quotRemInteger i0 i1 =
  (# quotInteger i0 i1, remInteger i0 i1 #)

decodeFloatInteger :: Float# -> (# Integer, Int# #)
decodeFloatInteger f =
  (# js_decodeFloatInteger_m f, js_decodeFloatInteger_n f #)

decodeDoubleInteger :: Double# -> (# Integer, Int# #)
decodeDoubleInteger d = (# js_decodeDoubleInteger_m d, js_decodeDoubleInteger_e d #)

oneInteger :: Integer
oneInteger = smallInteger 1#

instance Eq Integer where
  (==) = eqInteger
  (/=) = neqInteger

instance Ord Integer where
  (<=) = leInteger
  (>) = gtInteger
  (<) = ltInteger
  (>=) = geInteger
  compare = compareInteger

foreign import javascript "__asterius_jsffi.Integer.newInteger(${1})" js_newInteger :: Bool -> IO Integer

foreign import javascript "__asterius_jsffi.Integer.prependInteger(${1},${2})" js_prependInteger :: Integer -> Int -> IO ()

foreign import javascript "__asterius_jsffi.Integer.freezeInteger(${1})" js_freezeInteger :: Integer -> IO Integer

foreign import javascript "__asterius_jsffi.Integer.smallInteger(${1}, ${2})" js_smallInteger :: Word# -> Word# -> Integer

foreign import javascript "__asterius_jsffi.Integer.wordToInteger(${1}, ${2})" js_wordToInteger :: Word# -> Word# -> Integer

-- | Given integer and which 32-bit _piece_ of the word we want, return that piece.
foreign import javascript "__asterius_jsffi.Integer.integerToWord(${1}, ${2})" js_integerToWord :: Integer -> Int# -> Word#

foreign import javascript "__asterius_jsffi.Integer.plusInteger(${1},${2})" plusInteger :: Integer -> Integer -> Integer

foreign import javascript "__asterius_jsffi.Integer.minusInteger(${1},${2})" minusInteger :: Integer -> Integer -> Integer

foreign import javascript "__asterius_jsffi.Integer.timesInteger(${1},${2})" timesInteger :: Integer -> Integer -> Integer

foreign import javascript "__asterius_jsffi.Integer.negateInteger(${1})" negateInteger :: Integer -> Integer

foreign import javascript "__asterius_jsffi.Integer.eqInteger(${1},${2})" eqInteger :: Integer -> Integer -> Bool

foreign import javascript "__asterius_jsffi.Integer.neqInteger(${1},${2})" neqInteger :: Integer -> Integer -> Bool

foreign import javascript "__asterius_jsffi.Integer.absInteger(${1})" absInteger :: Integer -> Integer

foreign import javascript "__asterius_jsffi.Integer.signumInteger(${1})" signumInteger :: Integer -> Integer

foreign import javascript "__asterius_jsffi.Integer.leInteger(${1},${2})" leInteger :: Integer -> Integer -> Bool

foreign import javascript "__asterius_jsffi.Integer.gtInteger(${1},${2})" gtInteger :: Integer -> Integer -> Bool

foreign import javascript "__asterius_jsffi.Integer.ltInteger(${1},${2})" ltInteger :: Integer -> Integer -> Bool

foreign import javascript "__asterius_jsffi.Integer.geInteger(${1},${2})" geInteger :: Integer -> Integer -> Bool

foreign import javascript "__asterius_jsffi.Integer.quotInteger(${1},${2})" quotInteger :: Integer -> Integer -> Integer

foreign import javascript "__asterius_jsffi.Integer.remInteger(${1},${2})" remInteger :: Integer -> Integer -> Integer

foreign import javascript "__asterius_jsffi.Integer.encodeDoubleInteger(${1},${2})" encodeFloatInteger :: Integer -> Int# -> Float#

foreign import javascript "__asterius_jsffi.Integer.encode(__asterius_jsffi.FloatCBits.decodeDoubleInteger(${1})[0])" js_decodeFloatInteger_m :: Float# -> Integer

foreign import javascript "__asterius_jsffi.Integer.decodeDoubleInteger(${1})[1]" js_decodeFloatInteger_n :: Float# -> Int#

foreign import javascript "__asterius_jsffi.Integer.doubleFromInteger(${1})" floatFromInteger :: Integer -> Float#

foreign import javascript "__asterius_jsffi.Integer.encodeDoubleInteger(${1},${2})" encodeDoubleInteger :: Integer -> Int# -> Double#

foreign import javascript "__asterius_jsffi.Integer.encode(__asterius_jsffi.FloatCBits.decodeDoubleInteger(${1})[0])" js_decodeDoubleInteger_m :: Double# -> Integer

foreign import javascript "__asterius_jsffi.FloatCBits.decodeDoubleInteger(${1})[1]" js_decodeDoubleInteger_e :: Double# -> Int#

foreign import javascript "__asterius_jsffi.Integer.doubleFromInteger(${1})" doubleFromInteger :: Integer -> Double#

foreign import javascript "__asterius_jsffi.Integer.gcdInteger(${1},${2})" gcdInteger :: Integer -> Integer -> Integer

foreign import javascript "__asterius_jsffi.Integer.lcmInteger(${1},${2})" lcmInteger :: Integer -> Integer -> Integer

foreign import javascript "__asterius_jsffi.Integer.andInteger(${1},${2})" andInteger :: Integer -> Integer -> Integer

foreign import javascript "__asterius_jsffi.Integer.orInteger(${1},${2})" orInteger :: Integer -> Integer -> Integer

foreign import javascript "__asterius_jsffi.Integer.xorInteger(${1},${2})" xorInteger :: Integer -> Integer -> Integer

foreign import javascript "__asterius_jsffi.Integer.complementInteger(${1})" complementInteger :: Integer -> Integer

foreign import javascript "__asterius_jsffi.Integer.shiftLInteger(${1},${2})" shiftLInteger :: Integer -> Int# -> Integer

foreign import javascript "__asterius_jsffi.Integer.shiftRInteger(${1},${2})" shiftRInteger :: Integer -> Int# -> Integer

foreign import javascript "__asterius_jsffi.Integer.testBitInteger(${1},${2})" testBitInteger :: Integer -> Int# -> Bool

foreign import javascript "__asterius_jsffi.Integer.popCountInteger(${1})" popCountInteger :: Integer -> Int#

foreign import javascript "__asterius_jsffi.Integer.bitInteger(${1})" bitInteger :: Int# -> Integer

foreign import javascript "__asterius_jsffi.Integer.hashInteger(${1})" hashInteger :: Integer -> Int#

foreign import javascript "__asterius_jsffi.Integer.powInteger(${1},${2})" powInteger :: Integer -> Integer -> Integer
