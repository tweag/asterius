{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}

module Asterius.Resolve
  ( unresolvedGlobalRegType,
    LinkReport (..),
    linkStart,
  )
where

import Asterius.Binary.Orphans ()
import Asterius.Builtins
import Asterius.Internals.MagicNumber
import Asterius.JSFFI
import Asterius.MemoryTrap
import Asterius.Passes.DataSymbolTable
import Asterius.Passes.FunctionSymbolTable
import Asterius.Passes.GCSections
import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
import qualified Asterius.Types.SymbolSet as SS
import Asterius.Types.LinkReport
import Control.DeepSeq
import qualified Data.Map.Lazy as LM
import Foreign
import Language.Haskell.GHC.Toolkit.Constants

unresolvedGlobalRegType :: UnresolvedGlobalReg -> ValueType
unresolvedGlobalRegType gr = case gr of
  FloatReg _ -> F32
  DoubleReg _ -> F64
  _ -> I64

makeInfoTableSet :: AsteriusModule -> SM.SymbolMap Word32 -> [Int64]
makeInfoTableSet AsteriusModule {..} ss_off_map =
  map mkDataAddress $ SM.elems $ SM.restrictKeys ss_off_map $ SM.keysSet $
    SM.filter
      ((== InfoTable) . staticsType)
      staticsMap

resolveAsteriusModule ::
  Bool ->
  AsteriusModule ->
  ( Module,
    SM.SymbolMap Word32,
    SM.SymbolMap Word32,
    Int,
    Int
  )
resolveAsteriusModule debug m_globals_resolved =
  (new_mod, ss_off_map, fn_off_map, table_slots, initial_mblocks)
  where
    (fn_off_map, last_func_offset) = makeFunctionOffsetTable m_globals_resolved
    (ss_off_map, last_data_offset) = makeDataOffsetTable m_globals_resolved
    segs = makeMemory m_globals_resolved fn_off_map ss_off_map
    func_table = makeFunctionTable fn_off_map
    table_slots = fromIntegral $ tableBase + last_func_offset
    func_imports =
      rtsFunctionImports debug <> generateFFIFunctionImports (ffiMarshalState m_globals_resolved)
    new_function_map =
      LM.mapKeys entityName $ SM.toMap $ functionMap m_globals_resolved
    initial_pages =
      (fromIntegral (memoryBase + last_data_offset) `roundup` mblock_size)
        `quot` wasmPageSize
    initial_mblocks =
      initial_pages `quot` (mblock_size `quot` wasmPageSize)
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
        tableImport = TableImport
          { externalModuleName = "WasmTable",
            externalBaseName = "table"
          },
        tableSlots = table_slots,
        globalImports = rtsGlobalImports,
        globalExports = rtsGlobalExports,
        globalMap = globalsMap m_globals_resolved, -- Copy as-is.
        memorySegments = segs,
        memoryImport = MemoryImport
          { externalModuleName = "WasmMemory",
            externalBaseName = "memory"
          },
        memoryMBlocks = initial_mblocks
      }

linkStart ::
  Bool ->
  Bool ->
  AsteriusCachedModule ->
  SS.SymbolSet ->
  [EntitySymbol] ->
  (AsteriusModule, Module, LinkReport)
linkStart debug gc_sections store root_syms export_funcs =
  ( merged_m,
    result_m,
    LinkReport
      { staticsOffsetMap = ss_off_map,
        functionOffsetMap = fn_off_map,
        infoTableSet = makeInfoTableSet merged_m ss_off_map,
        Asterius.Types.LinkReport.tableSlots = tbl_slots,
        staticMBlocks = static_mbs,
        sptEntries = sptMap merged_m,
        bundledFFIMarshalState = ffiMarshalState merged_m
      }
  )
  where
    merged_m0
      | gc_sections = gcSections store root_syms export_funcs
      | otherwise = fromCachedModule store
    !merged_m0_evaluated = force merged_m0
    !merged_m
      | debug = addMemoryTrap merged_m0_evaluated
      | otherwise = merged_m0_evaluated
    (!result_m, !ss_off_map, !fn_off_map, !tbl_slots, !static_mbs) =
      resolveAsteriusModule debug merged_m
