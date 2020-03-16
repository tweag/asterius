module Asterius.Internals.MagicNumber
  ( dataTag,
    functionTag,
    invalidAddress,
  )
where

import Data.Int

dataTag :: Int64
dataTag = 2097143

functionTag :: Int64
functionTag = 2097133

invalidAddress :: Int64
invalidAddress = 0x1fffffffff0000
