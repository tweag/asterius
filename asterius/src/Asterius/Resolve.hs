{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Resolve
  ( unresolvedGlobalRegType,
    LinkReport (..),
    linkStart,
  )
where

import Asterius.Builtins
import Asterius.Internals.MagicNumber
import Asterius.JSFFI
import Asterius.MemoryTrap
import Asterius.Passes.DataSymbolTable
import Asterius.Passes.FunctionSymbolTable
import Asterius.Passes.GCSections
import Asterius.Types
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Lazy as LM
import qualified Data.Set as S
import Foreign
import GHC.Generics
import Language.Haskell.GHC.Toolkit.Constants
import Unsafe.Coerce

unresolvedGlobalRegType :: UnresolvedGlobalReg -> ValueType
unresolvedGlobalRegType gr = case gr of
  FloatReg _ -> F32
  DoubleReg _ -> F64
  _ -> I64

data LinkReport
  = LinkReport
      { staticsSymbolMap, functionSymbolMap :: LM.Map AsteriusEntitySymbol Int64,
        infoTableSet :: [Int64],
        tableSlots, staticMBlocks :: Int,
        sptEntries :: LM.Map AsteriusEntitySymbol (Word64, Word64),
        bundledFFIMarshalState :: FFIMarshalState
      }
  deriving (Generic, Show)

instance Binary LinkReport

instance Semigroup LinkReport where
  r0 <> r1 =
    LinkReport
      { staticsSymbolMap = staticsSymbolMap r0 <> staticsSymbolMap r1,
        functionSymbolMap = functionSymbolMap r0 <> functionSymbolMap r1,
        infoTableSet = infoTableSet r0 <> infoTableSet r1,
        tableSlots = 0,
        staticMBlocks = 0,
        sptEntries = sptEntries r0 <> sptEntries r1,
        bundledFFIMarshalState =
          bundledFFIMarshalState r0
            <> bundledFFIMarshalState r1
      }

instance Monoid LinkReport where
  mempty =
    LinkReport
      { staticsSymbolMap = mempty,
        functionSymbolMap = mempty,
        infoTableSet = mempty,
        tableSlots = 0,
        staticMBlocks = 0,
        sptEntries = mempty,
        bundledFFIMarshalState = mempty
      }

makeInfoTableSet ::
  AsteriusModule -> LM.Map AsteriusEntitySymbol Int64 -> [Int64]
makeInfoTableSet AsteriusModule {..} sym_map =
  LM.elems $ LM.restrictKeys sym_map $ LM.keysSet $
    LM.filter
      ((== InfoTable) . staticsType)
      staticsMap

resolveAsteriusModule ::
  Bool ->
  FFIMarshalState ->
  AsteriusModule ->
  Int64 ->
  Int64 ->
  ( Module,
    LM.Map AsteriusEntitySymbol Int64,
    LM.Map AsteriusEntitySymbol Int64,
    Int,
    Int
  )
resolveAsteriusModule debug bundled_ffi_state m_globals_resolved func_start_addr data_start_addr =
  (new_mod, ss_sym_map, func_sym_map, table_slots, initial_mblocks)
  where
    (func_sym_map, last_func_addr) =
      makeFunctionSymbolTable m_globals_resolved func_start_addr
    table_slots = fromIntegral $ last_func_addr .&. 0xFFFFFFFF
    func_table = makeFunctionTable func_sym_map func_start_addr
    (ss_sym_map, last_data_addr) =
      makeDataSymbolTable m_globals_resolved data_start_addr
    all_sym_map = func_sym_map <> ss_sym_map
    func_imports =
      rtsFunctionImports debug <> generateFFIFunctionImports bundled_ffi_state
    new_function_map = unsafeCoerce $ functionMap m_globals_resolved
    (initial_pages, segs) =
      makeMemory m_globals_resolved all_sym_map last_data_addr
    initial_mblocks =
      fromIntegral initial_pages `quot` (mblock_size `quot` wasmPageSize)
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
        tableExport = TableExport {externalName = "table"},
        tableSlots = table_slots,
        memorySegments = segs,
        memoryImport = MemoryImport
          { externalModuleName = "WasmMemory",
            externalBaseName = "memory"
          },
        memoryExport = MemoryExport {externalName = "memory"},
        memoryMBlocks = initial_mblocks
      }

linkStart ::
  Bool ->
  Bool ->
  Bool ->
  AsteriusModule ->
  S.Set AsteriusEntitySymbol ->
  [AsteriusEntitySymbol] ->
  (AsteriusModule, Module, LinkReport)
linkStart debug gc_sections verbose_err store root_syms export_funcs =
  ( merged_m,
    result_m,
    mempty
      { staticsSymbolMap = ss_sym_map,
        functionSymbolMap = func_sym_map,
        infoTableSet = makeInfoTableSet merged_m ss_sym_map,
        Asterius.Resolve.tableSlots = tbl_slots,
        staticMBlocks = static_mbs,
        sptEntries = sptMap merged_m,
        bundledFFIMarshalState = bundled_ffi_state
      }
  )
  where
    merged_m0
      | gc_sections = gcSections verbose_err store root_syms export_funcs
      | otherwise = store
    merged_m1
      | debug = addMemoryTrap merged_m0
      | otherwise = merged_m0
    !merged_m
      | verbose_err = merged_m1
      | otherwise =
        merged_m1
          { staticsMap =
              LM.filterWithKey
                ( \sym _ ->
                    not
                      ( "__asterius_barf_"
                          `BS.isPrefixOf` SBS.fromShort (entityName sym)
                      )
                )
                $ staticsMap merged_m1
          }
    bundled_ffi_state = ffiMarshalState merged_m
    (!result_m, !ss_sym_map, !func_sym_map, !tbl_slots, !static_mbs) =
      resolveAsteriusModule
        debug
        bundled_ffi_state
        merged_m
        -- reserve 0 for the null function pointer
        (1 .|. functionTag `shiftL` 32)
        -- leave 1KB empty for the --low-memory-unused optimization to work
        (0x00000400 .|. dataTag `shiftL` 32)
