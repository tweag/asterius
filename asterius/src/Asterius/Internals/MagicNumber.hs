module Asterius.Internals.MagicNumber
  ( dataTag,
    functionTag,
    invalidAddress,
    tableBase,
    memoryBase,
    mkDataAddress,
    mkFunctionAddress,
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
tableBase :: Word32
tableBase = 1

-- | Base address for data segments. NOTE: leave 1KB empty for the
-- @--low-memory-unused@ optimization to work.
memoryBase :: Word32
memoryBase = 1024

mkDataAddress :: Word32 -> Int64
mkDataAddress off = (dataTag `shiftL` 32) .|. fromIntegral (memoryBase + off)

mkFunctionAddress :: Word32 -> Int64
mkFunctionAddress off = (functionTag `shiftL` 32) .|. fromIntegral (tableBase + off)
