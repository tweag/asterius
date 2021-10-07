{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Asterius.Types.LinkReport
  ( LinkReport (..),
  )
where

import Asterius.Binary.TH
import Asterius.Types
import Asterius.Types.SymbolMap
import Data.Word

data LinkReport
  = LinkReport
      { staticsOffsetMap, functionOffsetMap :: SymbolMap Word32,
        infoTableOffsetSet :: [Word32],
        tableSlots :: Int,
        sptEntries :: SymbolMap (Word64, Word64),
        bundledFFIMarshalState :: FFIMarshalState,
        usedCCalls :: [String]
      }
  deriving (Show)

$(genBinary ''LinkReport)
