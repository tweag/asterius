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
      { symbolMap :: SymbolMap Word32,
        lastDataOffset :: Word32,
        tableSlots :: Int,
        bundledFFIMarshalState :: FFIMarshalState
      }
  deriving (Show)

$(genBinary ''LinkReport)
