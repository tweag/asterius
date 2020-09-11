module Asterius.Internals.ByteString
  ( intHex,
  )
where

import Asterius.Internals.SafeFromIntegral
import Data.ByteString.Builder
import Type.Reflection

{-# INLINE intHex #-}
intHex :: (Integral i, Show i, Typeable i) => i -> Builder
intHex x
  | x >= 0 =
    string7 "0x" <> wordHex (safeFromIntegral x)
  | otherwise =
    error $ "Asterius.Internals.ByteString.intHex: called with " <> show x
