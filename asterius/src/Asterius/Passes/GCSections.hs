{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Asterius.Passes.GCSections
  ( gcSections,
  )
where

import Asterius.Types
import Asterius.Types.EntitySymbol
import qualified Asterius.Types.SymbolMap as SM
import qualified Asterius.Types.SymbolSet as SS
import Data.Maybe

gcSections ::
  AsteriusCachedModule ->
  SS.SymbolSet ->
  [EntitySymbol] ->
  AsteriusModule
gcSections store_mod root_syms (SS.fromList -> export_funcs) =
  gcModule mod_syms export_funcs (fromCachedModule store_mod)
  where
    all_root_syms = root_syms <> module_exports
    mod_syms = resolveSyms all_root_syms $ dependencyMap store_mod
    module_exports =
      SS.fromList
        [ ffiExportClosure
          | FFIExportDecl {..} <-
              SM.elems $
                ffiExportDecls (ffiMarshalState $ fromCachedModule store_mod)
                  `SM.restrictKeys` export_funcs
        ]

-- | Resolve all symbols that are reachable from the given root symbols. This
-- includes symbols that refer to either statics or functions.
resolveSyms :: SS.SymbolSet -> SM.SymbolMap SS.SymbolSet -> SS.SymbolSet
resolveSyms root_syms dep_map = go (root_syms, SS.empty, mempty)
  where
    go (i_staging_syms, i_acc_syms, i_m_syms)
      | SS.null i_staging_syms = i_m_syms
      | otherwise =
        let o_acc_syms = i_staging_syms <> i_acc_syms
            (i_child_syms, o_m_syms) = SS.foldr' step (SS.empty, i_m_syms) i_staging_syms
            o_staging_syms = i_child_syms `SS.difference` o_acc_syms
         in go (o_staging_syms, o_acc_syms, o_m_syms)
      where
        step i_staging_sym (i_child_syms_acc, o_m_acc_syms)
          | Just es <- i_staging_sym `SM.lookup` dep_map =
            (es <> i_child_syms_acc, o_m_acc_syms <> SS.singleton i_staging_sym)
          | otherwise =
            (i_child_syms_acc, o_m_acc_syms)

-- | Given the reachable symbols (statics and functions) and the exported
-- functions for a module, garbage-collect all unreachable parts of the module.
gcModule :: SS.SymbolSet -> SS.SymbolSet -> AsteriusModule -> AsteriusModule
gcModule mod_syms export_funcs m =
  AsteriusModule
    { staticsMap = statics,
      functionMap = functions,
      ffiMarshalState =
        FFIMarshalState
          { ffiImportDecls = ffi_imports,
            ffiExportDecls = ffi_exports
          }
    }
  where
    statics = staticsMap m `SM.restrictKeys` mod_syms
    functions = functionMap m `SM.restrictKeys` mod_syms
    -- Since each JSFFI import comes in two parts (a function import and a
    -- wrapper function), we only keep the import whose wrapper function is
    -- used; the rest are definitely unreachable.
    wrapper_fn_syms = SS.fromList . catMaybes . map stripWrapperSuffix . SS.toList $ SM.keysSet functions
    ffi_imports = ffiImportDecls (ffiMarshalState m) `SM.restrictKeys` wrapper_fn_syms
    ffi_exports = ffiExportDecls (ffiMarshalState m) `SM.restrictKeys` export_funcs
