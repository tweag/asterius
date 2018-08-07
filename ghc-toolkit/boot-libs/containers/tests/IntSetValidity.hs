{-# LANGUAGE CPP #-}
module IntSetValidity (valid) where

import Data.Bits (xor, (.&.))
import Data.IntSet.Internal
import Test.QuickCheck (Property, counterexample, property, (.&&.))
import Utils.Containers.Internal.BitUtil (bitcount)

{--------------------------------------------------------------------
  Assertions
--------------------------------------------------------------------}
-- | Returns true iff the internal structure of the IntSet is valid.
valid :: IntSet -> Property
valid t =
  counterexample "nilNeverChildOfBin" (nilNeverChildOfBin t) .&&.
  counterexample "maskPowerOfTwo" (maskPowerOfTwo t) .&&.
  counterexample "commonPrefix" (commonPrefix t) .&&.
  counterexample "markRespected" (maskRespected t) .&&.
  counterexample "tipsValid" (tipsValid t)

-- Invariant: Nil is never found as a child of Bin.
nilNeverChildOfBin :: IntSet -> Bool
nilNeverChildOfBin t =
  case t of
    Nil -> True
    Tip _ _ -> True
    Bin _ _ l r -> noNilInSet l && noNilInSet r
  where
    noNilInSet t' =
      case t' of
        Nil -> False
        Tip _ _ -> True
        Bin _ _ l' r' -> noNilInSet l' && noNilInSet r'

-- Invariant: The Mask is a power of 2.  It is the largest bit position at which
--            two elements of the set differ.
maskPowerOfTwo :: IntSet -> Bool
maskPowerOfTwo t =
  case t of
    Nil -> True
    Tip _ _ -> True
    Bin _ m l r ->
      bitcount 0 (fromIntegral m) == 1 && maskPowerOfTwo l && maskPowerOfTwo r

-- Invariant: Prefix is the common high-order bits that all elements share to
--            the left of the Mask bit.
commonPrefix :: IntSet -> Bool
commonPrefix t =
  case t of
    Nil -> True
    Tip _ _ -> True
    b@(Bin p _ l r) -> all (sharedPrefix p) (elems b) && commonPrefix l && commonPrefix r
  where
    sharedPrefix :: Prefix -> Int -> Bool
    sharedPrefix p a = p == p .&. a

-- Invariant: In Bin prefix mask left right, left consists of the elements that
--            don't have the mask bit set; right is all the elements that do.
maskRespected :: IntSet -> Bool
maskRespected t =
  case t of
    Nil -> True
    Tip _ _ -> True
    Bin _ binMask l r ->
      all (\x -> zero x binMask) (elems l) &&
      all (\x -> not (zero x binMask)) (elems r) &&
      maskRespected l &&
      maskRespected r

-- Invariant: The Prefix is zero for the last 5 (on 32 bit arches) or 6 bits
--            (on 64 bit arches). The values of the set represented by a tip
--            are the prefix plus the indices of the set bits in the bit map.
--
-- Note: Valid entries stored in tip omitted.
tipsValid :: IntSet -> Bool
tipsValid t =
  case t of
    Nil -> True
    tip@(Tip p b) -> validTipPrefix p
    Bin _ _ l r -> tipsValid l && tipsValid r

validTipPrefix :: Prefix -> Bool
#if WORD_SIZE_IN_BITS==32
-- Last 5 bits of the prefix must be zero for 32 bit arches.
validTipPrefix p = (0x0000001F .&. p) == 0
#else
-- Last 6 bits of the prefix must be zero 64 bit anches.
validTipPrefix p = (0x000000000000003F .&. p) == 0
#endif
