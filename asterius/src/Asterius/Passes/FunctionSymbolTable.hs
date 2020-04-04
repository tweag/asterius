{-# LANGUAGE RecordWildCards #-}

module Asterius.Passes.FunctionSymbolTable
  ( makeFunctionSymbolTable,
    makeFunctionTable,
  )
where

import Asterius.Types
import Data.Bits
import Data.Coerce
import Data.Int
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Tuple

{-# INLINEABLE makeFunctionSymbolTable #-}
makeFunctionSymbolTable ::
  AsteriusModule -> Int64 -> (Map EntitySymbol Int64, Int64)
makeFunctionSymbolTable AsteriusModule {..} func_start_addr =
  swap $ Map.mapAccum (\a _ -> (succ a, a)) func_start_addr functionMap

{-# INLINEABLE makeFunctionTable #-}
makeFunctionTable :: Map EntitySymbol Int64 -> Int64 -> FunctionTable
makeFunctionTable func_sym_map func_start_addr = FunctionTable
  { tableFunctionNames = coerce $ Map.keys func_sym_map,
    tableOffset = fromIntegral $ func_start_addr .&. 0xFFFFFFFF
  }
