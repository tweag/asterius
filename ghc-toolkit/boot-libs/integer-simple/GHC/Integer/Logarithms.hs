{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash, UnboxedTuples, NoImplicitPrelude #-}
{-# LANGUAGE UnliftedFFITypes #-}

module GHC.Integer.Logarithms
    ( integerLogBase#
    , integerLog2#
    , wordLog2#
    ) where

import GHC.Prim
import GHC.Integer
import qualified GHC.Integer.Logarithms.Internals as I
import GHC.Types

#if !defined(ASTERIUS)
import Asterius.Magic
import GHC.Integer.Type (Integer(..))
#endif

-- | Calculate the integer logarithm for an arbitrary base.
--   The base must be greater than 1, the second argument, the number
--   whose logarithm is sought, should be positive, otherwise the
--   result is meaningless.
--
-- > base ^ integerLogBase# base m <= m < base ^ (integerLogBase# base m + 1)
--
-- for @base > 1@ and @m > 0@.
integerLogBase# :: Integer -> Integer -> Int#
#if !defined(ASTERIUS) && false
    XX
integerLogBase# (Integer b) (Integer m) = js_integerLogBase m b
#else
integerLogBase# b m = case step b of
                        (# _, e #) -> e
  where
    step pw =
      if m `ltInteger` pw
        then (# m, 0# #)
        else case step (pw `timesInteger` pw) of
               (# q, e #) ->
                 if q `ltInteger` pw
                   then (# q, 2# *# e #)
                   else (# q `quotInteger` pw, 2# *# e +# 1# #)
#endif

-- | Calculate the integer base 2 logarithm of an 'Integer'.
--   The calculation is more efficient than for the general case,
--   on platforms with 32- or 64-bit words much more efficient.
--
--  The argument must be strictly positive, that condition is /not/ checked.
integerLog2# :: Integer -> Int#
integerLog2# = I.integerLog2#

-- | This function calculates the integer base 2 logarithm of a 'Word#'.
wordLog2# :: Word# -> Int#
wordLog2# = I.wordLog2#

#if !defined(ASTERIUS)

foreign import javascript "__asterius_jsffi.Integer.integerLogBase(${1}, ${2})" js_integerLogBase :: Int# -> Int# -> Int#

#endif
