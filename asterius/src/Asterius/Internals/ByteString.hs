module Asterius.Internals.ByteString
  ( intHex
  ) where

import Data.ByteString.Builder

intHex :: Int -> Builder
intHex x
  | x >= 0 = string7 "0x" <> wordHex (fromIntegral x)
  | otherwise =
    error $ "Asterius.Internals.ByteString.intHex: called with " <> show x
