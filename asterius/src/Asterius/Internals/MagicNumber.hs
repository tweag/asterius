{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Asterius.Internals.MagicNumber
  ( dataTag
  , functionTag
  ) where

import Data.Int

dataTag, functionTag :: Int64
dataTag = 2097143

functionTag = 2097133
