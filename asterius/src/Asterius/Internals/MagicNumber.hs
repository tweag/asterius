{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Asterius.Internals.MagicNumber
  ( dataTag
  , functionTag
  , invalidSymbol
  ) where

import Data.Int

dataTag, functionTag, invalidSymbol :: Int64
dataTag = 2097143

functionTag = 2097133

invalidSymbol = 0xFFFFFFFFFFFF0000
