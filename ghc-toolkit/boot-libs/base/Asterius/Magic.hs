{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}

module Asterius.Magic
  ( accursedUnutterablePerformIO,
    unIO,
  )
where

import GHC.Magic
import GHC.Prim
import GHC.Types

{-# INLINE accursedUnutterablePerformIO #-}
accursedUnutterablePerformIO :: IO a -> a
accursedUnutterablePerformIO (IO m) = case runRW# m of
  (# _, a #) -> a

{-# INLINE unIO #-}
unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unIO (IO m) = m
