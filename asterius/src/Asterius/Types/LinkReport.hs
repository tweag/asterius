{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Asterius.Types.LinkReport
  ( LinkReport (..),
  )
where

import Asterius.Binary.TH
import Asterius.Types
import Asterius.Types.SymbolMap
import Data.Int
import Data.Word

data LinkReport
  = LinkReport
      { staticsOffsetMap, functionOffsetMap :: SymbolMap Word32,
        infoTableSet :: [Int64],
        tableSlots, staticMBlocks :: Int,
        sptEntries :: SymbolMap (Word64, Word64),
        bundledFFIMarshalState :: FFIMarshalState
      }
  deriving (Show)

$(genBinary ''LinkReport)
