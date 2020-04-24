{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Asterius.Passes.GCSections
  ( gcSections,
  )
where

import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
import qualified Asterius.Types.SymbolSet as SS
import Data.String

gcSections ::
  Bool ->
  AsteriusCachedModule ->
  SS.SymbolSet ->
  [EntitySymbol] ->
  AsteriusModule
gcSections verbose_err c_store_mod root_syms export_funcs =
  final_m
    { sptMap = spt_map,
      ffiMarshalState = ffi_this
    }
  where
    store_mod = fromCachedModule c_store_mod
    deps = dependencyMap c_store_mod
    spt_map =
      sptMap store_mod `SM.restrictKeys` SM.keysSet (staticsMap final_m)
    ffi_all = ffiMarshalState store_mod
    ffi_this =
      ffi_all
        { ffiImportDecls = flip SM.filterWithKey (ffiImportDecls ffi_all) $ \k _ ->
            (k <> "_wrapper") `SM.member` functionMap final_m,
          ffiExportDecls = ffi_exports
        }
    ffi_exports =
      ffiExportDecls (ffiMarshalState store_mod)
        `SM.restrictKeys` SS.fromList export_funcs
    root_syms' =
      SS.fromList [ffiExportClosure | FFIExportDecl {..} <- SM.elems ffi_exports]
        <> root_syms
    (_, _, final_m) = go (root_syms', SS.empty, mempty)
    go i@(i_staging_syms, _, _)
      | SS.null i_staging_syms = i
      | otherwise = go $ iter i
    iter (i_staging_syms, i_acc_syms, i_m) = (o_staging_syms, o_acc_syms, o_m)
      where
        o_acc_syms = i_staging_syms <> i_acc_syms
        (i_child_syms, o_m) =
          SS.foldr'
            ( \i_staging_sym (i_child_syms_acc, o_m_acc) ->
                if  | Just ss <- SM.lookup i_staging_sym (staticsMap store_mod),
                      es <- deps SM.! i_staging_sym -> -- should always succeed
                      ( es <> i_child_syms_acc,
                        o_m_acc
                          { staticsMap = SM.insert i_staging_sym ss (staticsMap o_m_acc)
                          }
                      )
                    | Just func <- SM.lookup i_staging_sym (functionMap store_mod),
                      es <- deps SM.! i_staging_sym -> -- should always succeed
                      ( es <> i_child_syms_acc,
                        o_m_acc
                          { functionMap =
                              SM.insert
                                i_staging_sym
                                func
                                (functionMap o_m_acc)
                          }
                      )
                    | verbose_err ->
                      ( i_child_syms_acc,
                        o_m_acc
                          { staticsMap =
                              SM.insert
                                ("__asterius_barf_" <> i_staging_sym)
                                AsteriusStatics
                                  { staticsType = ConstBytes,
                                    asteriusStatics =
                                      [ Serialized $
                                          entityName i_staging_sym
                                            <> ( case SM.lookup
                                                   i_staging_sym
                                                   (staticsErrorMap store_mod) of
                                                   Just err -> fromString (": " <> show err)
                                                   _ -> mempty
                                               )
                                            <> "\0"
                                      ]
                                  }
                                (staticsMap o_m_acc)
                          }
                      )
                    | otherwise ->
                      (i_child_syms_acc, o_m_acc)
            )
            (SS.empty, i_m)
            i_staging_syms
        o_staging_syms = i_child_syms `SS.difference` o_acc_syms
