{-# LANGUAGE RecordWildCards #-}

module Asterius.Passes.FunctionOffsetTable
  ( makeFunctionOffsetTable,
    makeFunctionTable,
  )
where

import Asterius.Internals.MagicNumber
import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
import Data.Tuple
import Data.Word

{-# INLINEABLE makeFunctionOffsetTable #-}
makeFunctionOffsetTable :: AsteriusModule -> (SM.SymbolMap Word32, Word32)
makeFunctionOffsetTable AsteriusModule {..} =
  swap $ SM.mapAccum (\a _ -> (succ a, a)) 0 functionMap

{-# INLINEABLE makeFunctionTable #-}
makeFunctionTable :: SM.SymbolMap Word32 -> FunctionTable
makeFunctionTable func_off_map = FunctionTable
  { tableFunctionNames = map entityName $ SM.keys func_off_map,
    tableOffset = ConstI32 $ fromIntegral tableBase -- TODO: WRONG (need to (dynamically) become 1).
  }
