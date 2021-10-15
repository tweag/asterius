module Asterius.Internals.MagicNumber
  ( invalidAddress,
    defaultTableBase,
    mkStaticDataAddress,
    mkStaticFunctionAddress,
  )
where

import Data.Int
import Data.Word

invalidAddress :: Int64
invalidAddress = 0x001fffffffff0000

-- | Base address for functions. NOTE: reserve 0 for the null function pointer.
defaultTableBase :: Word32
defaultTableBase = 1024

mkStaticDataAddress :: Word32 -> Word32 -> Int64
mkStaticDataAddress memory_base off = fromIntegral (memory_base + off)

mkStaticFunctionAddress :: Word32 -> Int64
mkStaticFunctionAddress off = fromIntegral (defaultTableBase + off)
