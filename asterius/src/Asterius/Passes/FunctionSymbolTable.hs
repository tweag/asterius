{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Passes.FunctionSymbolTable
  ( makeFunctionSymbolTable
  , makeFunctionTable
  ) where

import Asterius.Types
import Data.Coerce
import qualified Data.IntMap.Strict as IMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Tuple
import GHC.Exts
import GHC.Int

makeFunctionSymbolTable ::
     AsteriusModule -> Int64 -> (Map AsteriusEntitySymbol Int64, Int64)
makeFunctionSymbolTable AsteriusModule {..} l =
  swap $ Map.mapAccum (\a _ -> (succ a, a)) l functionMap

makeFunctionTable :: Map AsteriusEntitySymbol Int64 -> FunctionTable
makeFunctionTable sym_map =
  FunctionTable
    { functionNames =
        coerce $
        IMap.elems $
        Map.foldlWithKey'
          (\tot sym (I64# addr) -> IMap.insert (I# addr) sym tot)
          IMap.empty
          sym_map
    , tableExportName = "table"
    }
