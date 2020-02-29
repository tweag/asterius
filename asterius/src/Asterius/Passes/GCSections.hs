{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Asterius.Passes.GCSections
  ( gcSections,
  )
where

import Asterius.Types
import Data.Data
  ( Data,
    gmapQl,
  )
import qualified Data.Map.Lazy as LM
import qualified Data.Set as S
import Data.String
import Type.Reflection

gcSections ::
  Bool ->
  AsteriusModule ->
  S.Set AsteriusEntitySymbol ->
  [AsteriusEntitySymbol] ->
  AsteriusModule
gcSections verbose_err store_mod root_syms export_funcs =
  final_m
    { sptMap = spt_map,
      ffiMarshalState = ffi_this
    }
  where
    spt_map =
      LM.restrictKeys (sptMap store_mod) (LM.keysSet (staticsMap final_m))
    ffi_all = ffiMarshalState store_mod
    ffi_this =
      ffi_all
        { ffiImportDecls = flip LM.filterWithKey (ffiImportDecls ffi_all) $ \k _ ->
            (k <> "_wrapper") `LM.member` functionMap final_m,
          ffiExportDecls = ffi_exports
        }
    ffi_exports =
      ffiExportDecls (ffiMarshalState store_mod)
        `LM.restrictKeys` S.fromList export_funcs
    root_syms' =
      S.fromList [ffiExportClosure | FFIExportDecl {..} <- LM.elems ffi_exports]
        <> root_syms
    (_, _, final_m) = go (root_syms', S.empty, mempty)
    go i@(i_staging_syms, _, _)
      | S.null i_staging_syms = i
      | otherwise = go $ iter i
    iter (i_staging_syms, i_acc_syms, i_m) = (o_staging_syms, o_acc_syms, o_m)
      where
        o_acc_syms = i_staging_syms <> i_acc_syms
        (i_child_syms, o_m) =
          S.foldr'
            ( \i_staging_sym (i_child_syms_acc, o_m_acc) ->
                case LM.lookup i_staging_sym (staticsMap store_mod) of
                  Just ss ->
                    ( collectAsteriusEntitySymbols ss <> i_child_syms_acc,
                      o_m_acc
                        { staticsMap = LM.insert i_staging_sym ss (staticsMap o_m_acc)
                        }
                    )
                  _ -> case LM.lookup i_staging_sym (functionMap store_mod) of
                    Just func ->
                      ( collectAsteriusEntitySymbols func <> i_child_syms_acc,
                        o_m_acc
                          { functionMap =
                              LM.insert
                                i_staging_sym
                                func
                                (functionMap o_m_acc)
                          }
                      )
                    _
                      | verbose_err ->
                        ( i_child_syms_acc,
                          o_m_acc
                            { staticsMap =
                                LM.insert
                                  ("__asterius_barf_" <> i_staging_sym)
                                  AsteriusStatics
                                    { staticsType = ConstBytes,
                                      asteriusStatics =
                                        [ Serialized $
                                            entityName i_staging_sym
                                              <> ( case LM.lookup
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
            (S.empty, i_m)
            i_staging_syms
        o_staging_syms = i_child_syms `S.difference` o_acc_syms

collectAsteriusEntitySymbols :: Data a => a -> S.Set AsteriusEntitySymbol
collectAsteriusEntitySymbols t
  | Just HRefl <- eqTypeRep (typeOf t) (typeRep @AsteriusEntitySymbol) =
    S.singleton t
  | otherwise =
    gmapQl (<>) S.empty collectAsteriusEntitySymbols t
