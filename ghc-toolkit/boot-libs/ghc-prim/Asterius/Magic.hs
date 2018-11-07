{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UnboxedTuples #-}

module Asterius.Magic
  ( accursedUnutterableAddrToAny
  , accursedUnutterableAnyToAddr
  , accursedUnutterablePerformIO
  , unIO
  ) where

import GHC.Magic
import GHC.Prim
import GHC.Types

{-# INLINE accursedUnutterableAddrToAny #-}
accursedUnutterableAddrToAny :: Addr# -> (a :: TYPE r)
accursedUnutterableAddrToAny = unsafeCoerce#

{-# INLINE accursedUnutterableAnyToAddr #-}
accursedUnutterableAnyToAddr :: a -> Addr#
accursedUnutterableAnyToAddr a =
  runRW#
    (\s0 ->
       case anyToAddr# a s0 of
         (# _, addr #) -> addr)

{-# INLINE accursedUnutterablePerformIO #-}
accursedUnutterablePerformIO :: IO a -> a
accursedUnutterablePerformIO (IO m) =
  case runRW# m of
    (# _, a #) -> a

{-# INLINE unIO #-}
unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unIO (IO m) = m
