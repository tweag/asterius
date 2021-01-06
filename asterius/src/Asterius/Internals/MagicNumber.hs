module Asterius.Internals.MagicNumber
  ( invalidAddress,
    defaultTableBase,
    defaultMemoryBase,
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

-- | Base address for data segments. NOTE: leave 1KB empty for the
-- @--low-memory-unused@ optimization to work.
defaultMemoryBase :: Word32
defaultMemoryBase = 1024

mkStaticDataAddress :: Word32 -> Int64
mkStaticDataAddress off = fromIntegral (defaultMemoryBase + off)

mkStaticFunctionAddress :: Word32 -> Int64
mkStaticFunctionAddress off = fromIntegral (defaultTableBase + off)
