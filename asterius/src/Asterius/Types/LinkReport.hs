{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
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
        memoryBase, lastDataOffset :: Word32,
        tableSlots :: Int,
        bundledFFIMarshalState :: FFIMarshalState
      }
  deriving (Show)

$(genBinary ''LinkReport)
