{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Asterius.Types.LinkReport
  ( LinkReport (..),
  )
where

import Asterius.Binary.TH
import Asterius.Types
import Data.Int
import qualified Data.Map.Lazy as M
import Data.Word

data LinkReport
  = LinkReport
      { staticsSymbolMap, functionSymbolMap :: M.Map EntitySymbol Int64,
        infoTableSet :: [Int64],
        tableSlots, staticMBlocks :: Int,
        sptEntries :: M.Map EntitySymbol (Word64, Word64),
        bundledFFIMarshalState :: FFIMarshalState
      }
  deriving (Show)

$(genBinary ''LinkReport)

instance Semigroup LinkReport where
  r0 <> r1 =
    LinkReport
      { staticsSymbolMap = staticsSymbolMap r0 <> staticsSymbolMap r1,
        functionSymbolMap = functionSymbolMap r0 <> functionSymbolMap r1,
        infoTableSet = infoTableSet r0 <> infoTableSet r1,
        tableSlots = 0,
        staticMBlocks = 0,
        sptEntries = sptEntries r0 <> sptEntries r1,
        bundledFFIMarshalState =
          bundledFFIMarshalState r0
            <> bundledFFIMarshalState r1
      }

instance Monoid LinkReport where
  mempty =
    LinkReport
      { staticsSymbolMap = mempty,
        functionSymbolMap = mempty,
        infoTableSet = mempty,
        tableSlots = 0,
        staticMBlocks = 0,
        sptEntries = mempty,
        bundledFFIMarshalState = mempty
      }
