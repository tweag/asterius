{-# LANGUAGE RecordWildCards #-}

module Asterius.Passes.FunctionSymbolTable
  ( makeFunctionSymbolTable,
    makeFunctionTable,
  )
where

import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
import Data.Bits
import Data.Int
import Data.Tuple

{-# INLINEABLE makeFunctionSymbolTable #-}
makeFunctionSymbolTable ::
  AsteriusModule -> Int64 -> (SM.SymbolMap Int64, Int64)
makeFunctionSymbolTable AsteriusModule {..} func_start_addr =
  swap $ SM.mapAccum (\a _ -> (succ a, a)) func_start_addr functionMap

{-# INLINEABLE makeFunctionTable #-}
makeFunctionTable :: SM.SymbolMap Int64 -> Int64 -> FunctionTable
makeFunctionTable func_sym_map func_start_addr = FunctionTable
  { tableFunctionNames = map entityName $ SM.keys func_sym_map,
    tableOffset = fromIntegral $ func_start_addr .&. 0xFFFFFFFF
  }
