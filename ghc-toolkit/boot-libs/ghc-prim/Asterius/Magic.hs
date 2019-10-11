{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UnboxedTuples #-}

module Asterius.Magic
  ( accursedUnutterablePerformIO,
    unIO,
    unBool,
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

{-# INLINE unBool #-}
unBool :: Bool -> Int#
unBool False = 0#
unBool True = 1#
