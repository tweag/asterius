module IntMapValidity (valid) where

import Data.Bits (xor, (.&.))
import Data.IntMap.Internal
import Test.QuickCheck (Property, counterexample, property, (.&&.))
import Utils.Containers.Internal.BitUtil (bitcount)

{--------------------------------------------------------------------
  Assertions
--------------------------------------------------------------------}
-- | Returns true iff the internal structure of the IntMap is valid.
valid :: IntMap a -> Property
valid t =
  counterexample "nilNeverChildOfBin" (nilNeverChildOfBin t) .&&.
  counterexample "commonPrefix" (commonPrefix t) .&&.
  counterexample "maskRespected" (maskRespected t)

-- Invariant: Nil is never found as a child of Bin.
nilNeverChildOfBin :: IntMap a  -> Bool
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

-- Invariant: The Mask is a power of 2. It is the largest bit position at which
--            two keys of the map differ.
maskPowerOfTwo :: IntMap a -> Bool
maskPowerOfTwo t =
  case t of
    Nil -> True
    Tip _ _ -> True
    Bin _ m l r ->
      bitcount 0 (fromIntegral m) == 1 && maskPowerOfTwo l && maskPowerOfTwo r

-- Invariant: Prefix is the common high-order bits that all elements share to
--            the left of the Mask bit.
commonPrefix :: IntMap a -> Bool
commonPrefix t =
  case t of
    Nil -> True
    Tip _ _ -> True
    b@(Bin p _ l r) -> all (sharedPrefix p) (keys b) && commonPrefix l && commonPrefix r
  where
    sharedPrefix :: Prefix -> Int -> Bool
    sharedPrefix p a = p == p .&. a

-- Invariant: In Bin prefix mask left right, left consists of the elements that
--            don't have the mask bit set; right is all the elements that do.
maskRespected :: IntMap a -> Bool
maskRespected t =
  case t of
    Nil -> True
    Tip _ _ -> True
    Bin _ binMask l r ->
      all (\x -> zero x binMask) (keys l) &&
      all (\x -> not (zero x binMask)) (keys r) &&
      maskRespected l &&
      maskRespected r
