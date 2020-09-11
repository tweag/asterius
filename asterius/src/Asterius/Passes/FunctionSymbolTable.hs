{-# LANGUAGE RecordWildCards #-}

module Asterius.Passes.FunctionSymbolTable
  ( makeFunctionSymbolTable,
    makeFunctionTable,
  )
where

import Asterius.Internals.MagicNumber
import Asterius.Internals.SafeFromIntegral
import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
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
    tableOffset = ConstI32 $ safeFromIntegral $ unTag func_start_addr
  }
