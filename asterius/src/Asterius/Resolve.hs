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
import Asterius.Passes.DataOffsetTable
import Asterius.Passes.FunctionOffsetTable
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
  map mkStaticDataAddress $ SM.elems $ SM.restrictKeys ss_off_map $ SM.keysSet $
    SM.filter
      ((== InfoTable) . staticsType)
      staticsMap

resolveAsteriusModule ::
  Bool ->
  Bool ->
  AsteriusModule ->
  ( Module,
    AsteriusModule,
    SM.SymbolMap Word32,
    SM.SymbolMap Word32,
    Int,
    Int
  )
resolveAsteriusModule pic_on debug m_globals_resolved =
  (new_mod, final_m, ss_off_map, fn_off_map, table_slots, initial_mblocks)
  where
    -- Create the offset tables first. A dummy relocation function already
    -- exists in the input module so the maps will be created correctly.
    (fn_off_map, last_func_offset) = makeFunctionOffsetTable m_globals_resolved
    (ss_off_map, last_data_offset) = makeDataOffsetTable m_globals_resolved
    -- Create the data segments and the new relocation function second.
    -- We need to update the module with the real relocation function
    -- before we proceed.
    (segs, reloc_function) = makeMemory pic_on m_globals_resolved fn_off_map ss_off_map
    final_m = reloc_function <> m_globals_resolved
    -- Continue with the rest using the "real" module.
    func_table = makeFunctionTable fn_off_map
    table_slots = fromIntegral $ staticTableBase + last_func_offset
    func_imports =
      rtsFunctionImports debug <> generateFFIFunctionImports (ffiMarshalState final_m)
    new_function_map =
      LM.mapKeys entityName $ SM.toMap $ functionMap final_m
    initial_pages =
      (fromIntegral (staticMemoryBase + last_data_offset) `roundup` mblock_size)
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
        globalMap = globalsMap final_m, -- Copy as-is.
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
  Bool ->
  AsteriusCachedModule ->
  SS.SymbolSet ->
  [EntitySymbol] ->
  (AsteriusModule, Module, LinkReport)
linkStart pic_on debug gc_sections store root_syms export_funcs =
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
    !merged_m1
      | debug = addMemoryTrap merged_m0_evaluated
      | otherwise = merged_m0_evaluated
    (!result_m, !merged_m, !ss_off_map, !fn_off_map, !tbl_slots, !static_mbs) =
      resolveAsteriusModule pic_on debug merged_m1
