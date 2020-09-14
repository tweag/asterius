{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module Asterius.Internals.SafeFromIntegral where

import Type.Reflection

{-# INLINE safeFromIntegral #-}
safeFromIntegral ::
  forall a b.
  (Integral a, Integral b, Bounded b, Typeable a, Typeable b) =>
  a ->
  b
safeFromIntegral a
  | a_integer >= b_min_integer && a_integer <= b_max_integer =
    fromInteger a_integer
  | otherwise =
    error $
      "safeFromIntegral "
        <> show (typeRep @a)
        <> " "
        <> show (typeRep @b)
        <> " "
        <> show a_integer
  where
    a_integer = toInteger a
    b_min_integer = toInteger $ minBound @b
    b_max_integer = toInteger $ maxBound @b
