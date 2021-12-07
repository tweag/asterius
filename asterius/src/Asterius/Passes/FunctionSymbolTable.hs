{-# LANGUAGE RecordWildCards #-}

module Asterius.Passes.FunctionSymbolTable
  ( makeFunctionSymbolTable,
    makeFunctionTable,
  )
where

import Asterius.Internals.MagicNumber
import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
import Data.Word

{-# INLINEABLE makeFunctionSymbolTable #-}
makeFunctionSymbolTable :: AsteriusModule -> (SM.SymbolMap Word32, Word32)
makeFunctionSymbolTable AsteriusModule {..} = (fn_sym_map, last_func_addr - defaultTableBase)
 where
  (last_func_addr, fn_sym_map) = SM.mapAccum (\a _ -> (succ a, a)) defaultTableBase functionMap

{-# INLINEABLE makeFunctionTable #-}
makeFunctionTable :: SM.SymbolMap Word32 -> FunctionTable
makeFunctionTable func_off_map = FunctionTable
  { tableFunctionNames = map entityName $ SM.keys func_off_map,
    tableOffset = ConstI32 $ fromIntegral defaultTableBase
  }
