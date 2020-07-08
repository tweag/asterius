{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :  Asterius.Passes.GCSections
-- Copyright   :  (c) 2018 EURL Tweag
-- License     :  All rights reserved (see LICENCE file in the distribution).
--
-- Given a set of root symbols and exported functions, load an 'AsteriusModule'
-- from disk, while eliminating unreachable code ('gcSections'). If the
-- complete module is needed, use 'fromAsteriusRepModule' instead.
module Asterius.Passes.GCSections
  ( gcSections,
    fromAsteriusRepModule,
  )
where

import Asterius.Ar
import Asterius.Binary.NameCache
import Asterius.Types
import Asterius.Types.EntitySymbol
import qualified Asterius.Types.SymbolMap as SM
import qualified Asterius.Types.SymbolSet as SS
import Control.DeepSeq
import Data.Maybe
import qualified Data.Set as Set
import Data.Traversable

-- | Build an 'AsteriusModule' from an 'AsteriusRepModule', by keeping only the
-- parts of the program that are reachable from the given root symbols and
-- exported functions. Notice that this operation needs to be in 'IO', since
-- most parts of the generated 'AsteriusModule' need to be read from disk.
gcSections ::
  Bool ->
  AsteriusRepModule ->
  SS.SymbolSet ->
  [EntitySymbol] ->
  IO AsteriusModule
gcSections verbose_err module_rep root_syms export_funcs =
  buildGCModule mod_syms err_syms module_rep (SS.fromList export_funcs)
  where
    all_root_syms = root_syms <> moduleExports module_rep
    (force -> !mod_syms, err_syms) =
      resolveSyms verbose_err all_root_syms $
        dependencyMap module_rep

-- | Resolve all symbols that are reachable from the given root symbols. This
-- includes 2 categories: symbols that refer to statics and functions, and
-- symbols that refer to statics originating from barf messages (when
-- @verbose_err@ is set to @True@).
resolveSyms :: Bool -> SS.SymbolSet -> SM.SymbolMap SS.SymbolSet -> (SS.SymbolSet, SS.SymbolSet)
resolveSyms verbose_err root_syms dep_map = go (root_syms, SS.empty, mempty, mempty)
  where
    go (i_staging_syms, i_acc_syms, i_m_syms, i_err_syms)
      | SS.null i_staging_syms = (i_m_syms, i_err_syms)
      | otherwise =
        let o_acc_syms = i_staging_syms <> i_acc_syms
            (i_child_syms, o_m_syms, o_err_syms) = SS.foldr' step (SS.empty, i_m_syms, i_err_syms) i_staging_syms
            o_staging_syms = i_child_syms `SS.difference` o_acc_syms
         in go (o_staging_syms, o_acc_syms, o_m_syms, o_err_syms)
      where
        step i_staging_sym (i_child_syms_acc, o_m_acc_syms, err_syms)
          | Just es <- i_staging_sym `SM.lookup` dep_map =
            (es <> i_child_syms_acc, o_m_acc_syms <> SS.singleton i_staging_sym, err_syms)
          | verbose_err =
            (i_child_syms_acc, o_m_acc_syms, err_syms <> SS.singleton i_staging_sym)
          | otherwise =
            (i_child_syms_acc, o_m_acc_syms, err_syms)

-- | Given the reachable symbols (statics and functions) and the exported
-- functions for a module, garbage-collect all unreachable parts of the module.
gcModule :: SS.SymbolSet -> SS.SymbolSet -> AsteriusModule -> AsteriusModule
gcModule mod_syms export_funcs m =
  AsteriusModule
    { staticsMap = statics,
      functionMap = functions,
      globalsMap = globals,
      sptMap = spt_map,
      ffiMarshalState =
        FFIMarshalState
          { ffiImportDecls = ffi_imports,
            ffiExportDecls = ffi_exports
          }
    }
  where
    statics = staticsMap m `SM.restrictKeys` mod_syms
    functions = functionMap m `SM.restrictKeys` mod_syms
    globals = globalsMap m `SM.restrictKeys` mod_syms
    spt_map = sptMap m `SM.restrictKeys` mod_syms
    -- Since each JSFFI import comes in two parts (a function import and a
    -- wrapper function), we only keep the import whose wrapper function is
    -- used; the rest are definitely unreachable.
    wrapper_fn_syms = SS.fromList . catMaybes . map stripWrapperSuffix . SS.toList $ SM.keysSet functions
    ffi_imports = ffiImportDecls (ffiMarshalState m) `SM.restrictKeys` wrapper_fn_syms
    ffi_exports = ffiExportDecls (ffiMarshalState m) `SM.restrictKeys` export_funcs

-- | NOTE: This function only loads object and archive files specified by
-- @accessed_files@, and filters those __per module__. The correctness of the
-- approach thus depends on two important assumptions: (a) for each JSFFI
-- import, @<name>@ and @<name>_wrapper@ are always in the same module, and (b)
-- the keys of the @sptMap@ refer only to statics in the same module.
loadFilterEverything :: AsteriusRepModule -> (AsteriusModule -> AsteriusModule) -> IO AsteriusModule
loadFilterEverything AsteriusRepModule {..} fn = do
  ncu <- newNameCacheUpdater
  objs <- for (Set.toList objectSources) $ \path ->
    fn <$> loadObjectFile ncu path
  arcs <- for (Set.toList archiveSources) $ \path ->
    loadArchiveFile ncu path fn
  pure $ fn inMemoryModule <> mconcat objs <> mconcat arcs

-- | Convert an 'AsteriusRepModule' to a self-contained 'AsteriusModule' by
-- loading everything remaining from disk and combining it with the parts of
-- 'AsteriusModule' we have in memory (in 'inMemoryModule'). This function is
-- useful if the entirety of the module is needed (@gcSections@ is set to
-- @False@). Otherwise, use @loadFilterEverything@ instead.
fromAsteriusRepModule :: AsteriusRepModule -> IO AsteriusModule
fromAsteriusRepModule AsteriusRepModule {..} = do
  ncu <- newNameCacheUpdater
  objs <- for (Set.toList objectSources) $ loadObjectFile ncu
  arcs <- for (Set.toList archiveSources) $ loadCompleteArchiveFile ncu
  pure $ inMemoryModule <> mconcat objs <> mconcat arcs

buildGCModule :: SS.SymbolSet -> SS.SymbolSet -> AsteriusRepModule -> SS.SymbolSet -> IO AsteriusModule
buildGCModule mod_syms err_syms module_rep export_funcs = do
  everything <- loadFilterEverything module_rep (gcModule mod_syms export_funcs)
  pure everything {staticsMap = staticsMap everything <> err_statics}
  where
    err_statics =
      SM.fromList
        [ ( "__asterius_barf_" <> sym,
            AsteriusStatics
              { staticsType = ConstBytes,
                asteriusStatics = [Serialized $ entityName sym <> "\0"]
              }
          )
          | sym <- SS.toList err_syms
        ]
