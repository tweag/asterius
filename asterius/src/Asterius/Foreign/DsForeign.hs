{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Asterius.Foreign.DsForeign
  ( asteriusDsForeigns,
  )
where

import Asterius.Foreign.Internals
import Asterius.Foreign.SupportedTypes
import Asterius.Foreign.TypesTag
import Asterius.Types
import Control.Monad
import Data.List
import Data.Maybe
import DsCCall
import DsForeign
import DsMonad
import ForeignCall
import GhcPlugins
import HsSyn
import IfaceEnv
import MkId
import OrdList
import Pair
import PrelNames
import TcRnMonad
import TcType
import TysPrim
import Prelude hiding ((<>))

asteriusDsForeigns ::
  [LForeignDecl GhcTc] -> DsM (ForeignStubs, OrdList Binding)
asteriusDsForeigns [] = return (NoStubs, nilOL)
asteriusDsForeigns fos = do
  bindss <- mapM do_ldecl fos
  return (NoStubs, foldr (appOL . toOL) nilOL bindss)
  where
    do_ldecl (L loc decl) = putSrcSpanDs loc (do_decl decl)
    do_decl ForeignImport {fd_name = id, fd_i_ext = co, fd_fi = spec} = do
      traceIf (text "fi start" <+> ppr id)
      let id' = unLoc id
      bs <- asteriusDsFImport id' co spec
      traceIf (text "fi end" <+> ppr id)
      return bs
    do_decl ForeignExport {} = return []
    do_decl (XForeignDecl _) = panic "asteriusDsForeigns"

asteriusDsFImport :: Id -> Coercion -> ForeignImport -> DsM [Binding]
asteriusDsFImport id co (CImport cconv safety mHeader spec (unLoc -> src)) =
  asteriusDsCImport id co spec (unLoc cconv) (unLoc safety) mHeader src

asteriusDsCImport ::
  Id ->
  Coercion ->
  CImportSpec ->
  CCallConv ->
  Safety ->
  Maybe Header ->
  SourceText ->
  DsM [Binding]
asteriusDsCImport id co (CFunction target) JavaScriptCallConv safety _ _ =
  asteriusDsFCall id co (CCall (CCallSpec target JavaScriptCallConv safety))
asteriusDsCImport id co CWrapper JavaScriptCallConv _ _ src =
  asteriusDsFExportDynamic id co src
asteriusDsCImport id co spec cconv safety mHeader _ = do
  (r, _, _) <- dsCImport id co spec cconv safety mHeader
  pure r

asteriusDsFCall :: Id -> Coercion -> ForeignCall -> DsM [(Id, Expr TyVar)]
asteriusDsFCall fn_id co fcall = do
  dflags <- getDynFlags
  jsval_tycon_name <- lookupOrig jsValModule jsValTyConOccName
  jsval_tycon <- lookupTyCon jsval_tycon_name
  let jsval_ty = mkTyConTy jsval_tycon
  (jsvalzh_ty, jsfunc_ccall_res_wrapper) <- asteriusBoxResult jsval_ty
  jsffi_imp_mk_fcall_uniq <- newUnique
  let ty = pFst $ coercionKind co
      Right FFIFunctionType {..} = getFFIFunctionType ty
      jsffi_imp_mk_fcall =
        CCall $
          CCallSpec
            ( StaticTarget
                (SourceText "__asterius_jsffi_imp_mk")
                "__asterius_jsffi_imp_mk"
                Nothing
                True
            )
            CCallConv
            PlayRisky
      jsffi_imp_key_rhs =
        jsfunc_ccall_res_wrapper $
          mkFCall
            dflags
            jsffi_imp_mk_fcall_uniq
            jsffi_imp_mk_fcall
            [ mkStringLit $ read src_txt,
              mkWordLitWord dflags $ ffiValueTypesTag ffiParamTypes,
              mkWordLitWord dflags $ ffiValueTypesTag ffiResultTypes
            ]
            jsvalzh_ty
      (CCall (CCallSpec (StaticTarget (SourceText src_txt) _ _ _) _ _)) = fcall
  jsffi_imp_key_id <-
    (`setInlinePragma` neverInlinePragma)
      <$> mkSysLocalM (fsLit "__asterius_jsffi") jsval_ty
  (jsffi_imp_key_raw, jsffi_imp_key_raw_case) <-
    asteriusUnboxArg $
      Var jsffi_imp_key_id
  jsffi_imp_call_fcall_uniq <- newUnique
  let jsffi_imp_call_fcall =
        CCall $
          CCallSpec
            ( StaticTarget
                (SourceText "__asterius_jsffi_imp_call")
                "__asterius_jsffi_imp_call"
                Nothing
                True
            )
            CCallConv
            PlayRisky
  let (tv_bndrs, rho) = tcSplitForAllVarBndrs ty
      (arg_tys, io_res_ty) = tcSplitFunTys rho
  args <- newSysLocalsDs arg_tys
  (val_args, arg_wrappers) <- mapAndUnzipM asteriusUnboxArg (map Var args)
  (ccall_result_ty, res_wrapper) <- asteriusBoxResult io_res_ty
  let tvs = map binderVar tv_bndrs
      the_ccall_app =
        jsffi_imp_key_raw_case $
          mkFCall
            dflags
            jsffi_imp_call_fcall_uniq
            jsffi_imp_call_fcall
            (jsffi_imp_key_raw : val_args)
            ccall_result_ty
      wrapper_body = foldr ($) (res_wrapper the_ccall_app) arg_wrappers
      wrap_rhs = mkLams (tvs ++ args) wrapper_body
      wrap_rhs' = Cast wrap_rhs co
  pure
    [ (jsffi_imp_key_id, jsffi_imp_key_rhs),
      (fn_id, wrap_rhs')
    ]

asteriusDsFExportDynamic :: Id -> Coercion -> SourceText -> DsM [Binding]
asteriusDsFExportDynamic id co0 src = do
  dflags <- getDynFlags
  cback <- newSysLocalDs arg_ty
  newStablePtrId <- dsLookupGlobalId newStablePtrName
  stable_ptr_tycon <- dsLookupTyCon stablePtrTyConName
  let stable_ptr_ty = mkTyConApp stable_ptr_tycon [arg_ty]
  bindIOId <- dsLookupGlobalId bindIOName
  stbl_value <- newSysLocalDs stable_ptr_ty
  let adj_args =
        [ Var stbl_value,
          mkIntLitInt dflags (fromIntegral ffi_params_tag),
          mkIntLitInt dflags (fromIntegral ffi_ret_tag),
          mkIntLitInt dflags (if ffiInIO then 1 else 0),
          mkIntLitInt dflags (if oneshot then 1 else 0)
        ]
      new_hs_callback = fsLit "newHaskellCallback"
  ccall_adj <-
    dsCCall
      new_hs_callback
      adj_args
      PlayRisky
      (mkTyConApp io_tc [res_ty])
  let io_app =
        mkLams tvs $
          Lam cback $
            mkApps
              (Var bindIOId)
              [ Type stable_ptr_ty,
                Type res_ty,
                mkApps (Var newStablePtrId) [Type arg_ty, Var cback],
                Lam stbl_value ccall_adj
              ]
      fed = (id `setInlineActivation` NeverActive, Cast io_app co0)
  return [fed]
  where
    oneshot
      | src == SourceText "\"wrapper\"" = False
      | src == SourceText "\"wrapper oneshot\"" = True
      | otherwise = error "asteriusDsFExportDynamic"
    ty = pFst (coercionKind co0)
    (tvs, sans_foralls) = tcSplitForAllTys ty
    ([arg_ty], fn_res_ty) = tcSplitFunTys sans_foralls
    Just (io_tc, res_ty) = tcSplitIOType_maybe fn_res_ty
    Right FFIFunctionType {..} = getFFIFunctionType arg_ty
    ffi_params_tag = ffiValueTypesTag ffiParamTypes
    ffi_ret_tag = ffiValueTypesTag ffiResultTypes

asteriusUnboxArg :: CoreExpr -> DsM (CoreExpr, CoreExpr -> CoreExpr)
asteriusUnboxArg arg
  | isAnyTy arg_ty = return (arg, id)
  | otherwise = unboxArg arg
  where
    arg_ty = exprType arg

asteriusBoxResult :: Type -> DsM (Type, CoreExpr -> CoreExpr)
asteriusBoxResult result_ty
  | Just (io_tycon, io_res_ty) <- tcSplitIOType_maybe result_ty = do
    res <- asteriusResultWrapper io_res_ty
    let extra_result_tys = case res of
          (Just ty, _)
            | isUnboxedTupleType ty ->
              let Just ls = tyConAppArgs_maybe ty in tail ls
          _ -> []
        return_result state anss =
          mkCoreUbxTup
            (realWorldStatePrimTy : io_res_ty : extra_result_tys)
            (state : anss)
    (ccall_res_ty, the_alt) <- mk_alt return_result res
    state_id <- newSysLocalDs realWorldStatePrimTy
    let io_data_con = head (tyConDataCons io_tycon)
        toIOCon = dataConWrapId io_data_con
        wrap the_call =
          mkApps
            (Var toIOCon)
            [ Type io_res_ty,
              Lam state_id $
                mkWildCase
                  (App the_call (Var state_id))
                  ccall_res_ty
                  (coreAltType the_alt)
                  [the_alt]
            ]
    return (realWorldStatePrimTy `mkFunTy` ccall_res_ty, wrap)
asteriusBoxResult result_ty = do
  res <- asteriusResultWrapper result_ty
  (ccall_res_ty, the_alt) <- mk_alt return_result res
  let wrap the_call =
        mkWildCase
          (App the_call (Var realWorldPrimId))
          ccall_res_ty
          (coreAltType the_alt)
          [the_alt]
  return (realWorldStatePrimTy `mkFunTy` ccall_res_ty, wrap)
  where
    return_result _ [ans] = ans
    return_result _ _ = panic "return_result: expected single result"

asteriusResultWrapper :: Type -> DsM (Maybe Type, CoreExpr -> CoreExpr)
asteriusResultWrapper result_ty
  | isAnyTy result_ty = return (Just result_ty, id)
  | otherwise = resultWrapper result_ty
