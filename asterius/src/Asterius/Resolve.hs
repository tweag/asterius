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

makeInfoTableOffsetSet :: AsteriusModule -> SM.SymbolMap Word32 -> [Word32]
makeInfoTableOffsetSet AsteriusModule {..} ss_off_map =
  SM.elems $ SM.restrictKeys ss_off_map $ SM.keysSet $
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
resolveAsteriusModule pic_is_on debug m_globals_resolved =
  (new_mod, final_m, ss_off_map, fn_off_map, table_slots, initial_bytes)
  where
    -- Create the function offset table first. A dummy relocation function
    -- already so the map will be created correctly.
    (fn_off_map, last_func_offset) = makeFunctionOffsetTable m_globals_resolved
    -- Create the data segments, the statics offset table, and the final module
    -- next. If we are generating position independent code (i.e. @--pic@ is
    -- on), the relocation function must be replaced, and new data segments
    -- (and corresponding statics) must be added, to hold the offsets needed by
    -- the relocation function. All this is handled by @makeMemory@.
    (segs, ss_off_map, last_data_offset, final_m) = makeMemory pic_is_on m_globals_resolved fn_off_map
    func_table = makeFunctionTable fn_off_map
    table_slots = fromIntegral last_func_offset
    func_imports =
      rtsFunctionImports debug <> generateFFIFunctionImports (ffiMarshalState final_m)
    new_function_map =
      LM.mapKeys entityName $ SM.toMap $ functionMap final_m
    initial_bytes = fromIntegral last_data_offset
    initial_mblocks = -- minimum limit
      (fromIntegral last_data_offset `roundup` mblock_size) `quot` mblock_size
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
        infoTableOffsetSet = makeInfoTableOffsetSet merged_m ss_off_map,
        Asterius.Types.LinkReport.tableSlots = tbl_slots,
        staticBytes = static_bytes,
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
    (!result_m, !merged_m, !ss_off_map, !fn_off_map, !tbl_slots, !static_bytes) =
      resolveAsteriusModule pic_on debug merged_m1
