{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Resolve
  ( unresolvedGlobalRegType,
    LinkReport (..),
    linkStart,
  )
where

import Asterius.Binary.Orphans ()
import Asterius.Builtins
import Asterius.JSFFI
import Asterius.MemoryTrap
import Asterius.Passes.DataOffsetTable
import Asterius.Passes.FindCCall
import Asterius.Passes.FunctionOffsetTable
import Asterius.Passes.GCSections
import Asterius.Passes.Tracing
import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
import qualified Asterius.Types.SymbolSet as SS
import Asterius.Types.LinkReport
import Control.DeepSeq
import qualified Data.Map.Lazy as LM
import Data.String
import Foreign

unresolvedGlobalRegType :: UnresolvedGlobalReg -> ValueType
unresolvedGlobalRegType gr = case gr of
  LongReg _ -> I64
  FloatReg _ -> F32
  DoubleReg _ -> F64
  _ -> I32

resolveAsteriusModule ::
  Bool ->
  AsteriusModule ->
  ( Module,
    AsteriusModule,
    SM.SymbolMap Word32,
    SM.SymbolMap Word32,
    Word32,
    Word32,
    Int
  )
resolveAsteriusModule debug m_globals_resolved =
  (new_mod, final_m, ss_off_map, fn_off_map, memory_base, last_data_offset, table_slots)
  where
    -- Create the function offset table first. A dummy relocation function
    -- already so the map will be created correctly.
    (fn_off_map, last_func_offset) = makeFunctionOffsetTable m_globals_resolved
    -- Create the data segments, the statics offset table, and the final module
    -- next. If we are generating position independent code (i.e. @--pic@ is
    -- on), the relocation function must be replaced, and new data segments
    -- (and corresponding statics) must be added, to hold the offsets needed by
    -- the relocation function. All this is handled by @makeMemory@.
    (segs, ss_off_map, memory_base, last_data_offset, final_m) = makeMemory m_globals_resolved fn_off_map
    func_table = makeFunctionTable fn_off_map
    table_slots = fromIntegral last_func_offset
    func_imports =
      rtsFunctionImports debug <> generateFFIFunctionImports (ffiMarshalState final_m)
    new_function_map =
      LM.mapKeys entityName $ SM.toMap $ functionMap final_m
    new_mod = Module
      { functionMap' = new_function_map,
        functionImports = func_imports,
        functionExports =
          filter
            ( \FunctionExport {internalName = e} ->
                e
                  `LM.member` new_function_map
                  || any
                    ( \FunctionImport {internalName = i} ->
                        i == e
                    )
                    func_imports
            )
            $ rtsFunctionExports debug,
        functionTable = func_table,
        tableImport = Nothing,
        tableSlots = table_slots,
        memorySegments = segs,
        memoryImport = Nothing
      }

linkStart ::
  Bool ->
  AsteriusCachedModule ->
  SS.SymbolSet ->
  [EntitySymbol] ->
  (Module, LinkReport)
linkStart debug store root_syms export_funcs =
  ( result_m,
    LinkReport
      { staticsOffsetMap = ss_off_map,
        functionOffsetMap = fn_off_map,
        memoryBase = memory_base,
        lastDataOffset = last_data_offset,
        Asterius.Types.LinkReport.tableSlots = tbl_slots,
        bundledFFIMarshalState = ffiMarshalState merged_m
      }
  )
  where
    merged_m0 = gcSections store root_syms export_funcs
    !merged_m0_evaluated = force merged_m0
    !merged_m1
      | debug = traceModule $ addMemoryTrap merged_m0_evaluated
      | otherwise = merged_m0_evaluated
    (!result_m, !merged_m, !ss_off_map, !fn_off_map, !memory_base, !last_data_offset, !tbl_slots) =
      resolveAsteriusModule debug merged_m1
