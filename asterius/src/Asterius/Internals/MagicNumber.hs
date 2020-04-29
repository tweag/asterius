module Asterius.Internals.MagicNumber
  ( dataTag,
    functionTag,
    invalidAddress,
  )
where

import Data.Int

dataTag :: Int64
dataTag = 0x00000000001ffff7 -- 2097143

functionTag :: Int64
functionTag = 0x00000000001fffed -- 2097133

invalidAddress :: Int64
invalidAddress = 0x001fffffffff0000
