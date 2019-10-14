module Asterius.Internals.MagicNumber
  ( dataTag,
    functionTag,
    invalidAddress,
  )
where

import Data.Int

dataTag, functionTag, invalidAddress :: Int64
dataTag = 2097143
functionTag = 2097133
invalidAddress = 0x1fffffffff0000
