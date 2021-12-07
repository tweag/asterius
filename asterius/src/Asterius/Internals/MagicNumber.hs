module Asterius.Internals.MagicNumber
  ( invalidAddress,
    defaultTableBase,
  )
where

import Data.Int
import Data.Word

invalidAddress :: Int32
invalidAddress = 0x001fffffffff0000

-- | Base address for functions. NOTE: reserve 0 for the null function pointer.
defaultTableBase :: Word32
defaultTableBase = 1024

