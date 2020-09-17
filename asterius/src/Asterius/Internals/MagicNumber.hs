module Asterius.Internals.MagicNumber
  ( dataTag,
    functionTag,
    invalidAddress,
    staticTableBase,
    staticMemoryBase,
    mkStaticDataAddress,
    mkStaticFunctionAddress,
  )
where

import Data.Bits
import Data.Int
import Data.Word

dataTag :: Int64
dataTag = 0x00000000001ffff7 -- 2097143

functionTag :: Int64
functionTag = 0x00000000001fffed -- 2097133

invalidAddress :: Int64
invalidAddress = 0x001fffffffff0000

-- | Base address for functions. NOTE: reserve 0 for the null function pointer.
staticTableBase :: Word32
staticTableBase = 1

-- | Base address for data segments. NOTE: leave 1KB empty for the
-- @--low-memory-unused@ optimization to work.
staticMemoryBase :: Word32
staticMemoryBase = 1024

mkStaticDataAddress :: Word32 -> Int64
mkStaticDataAddress off = (dataTag `shiftL` 32) .|. fromIntegral (staticMemoryBase + off)

mkStaticFunctionAddress :: Word32 -> Int64
mkStaticFunctionAddress off = (functionTag `shiftL` 32) .|. fromIntegral (staticTableBase + off)
