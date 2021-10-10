{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Asterius.Foreign.DsForeign
  ( asteriusDsForeigns,
  )
where

import Asterius.Foreign.ExportStatic
import Asterius.Foreign.Internals
import Asterius.Types
import Control.Monad
import CoreUnfold
import Data.List
import Data.Maybe
import DsCCall
import DsForeign
import DsMonad
import ForeignCall
import GHC.Hs
import GhcPlugins
import MkId
import OrdList
import Pair
import PrelNames
import RepType
import TcEnv
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
    do_decl (XForeignDecl nec) = noExtCon nec

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
-- TODO: special treatment for prim call conv?
-- TODO: add arg/return primreps?
-- see https://github.com/ghc/ghc/commit/ff04eb5973b69fcc60e7d0945a74becd068c1888#diff-c101aeb1e72786c2c6e5035cda85ebc8369c91bbd0daba3abaa21b508d7156f8R103
asteriusDsCImport id co (CFunction target) cconv safety _ _ | cconv /= PrimCallConv =
  asteriusDsFCall id co (CCall (mkCCallSpec target cconv safety (panic "Missing Return PrimRep") (panic "Missing Argument PrimReps")))
asteriusDsCImport id co CWrapper JavaScriptCallConv _ _ src =
  asteriusDsFExportDynamic id co src
asteriusDsCImport id co spec cconv safety mHeader _ = do
  (r, _, _) <- dsCImport id co spec cconv safety mHeader
  pure r

asteriusDsFCall :: Id -> Coercion -> ForeignCall -> DsM [(Id, Expr TyVar)]
asteriusDsFCall fn_id co (CCall (CCallSpec target cconv safety _ _)) = do
  let ty = pFst $ coercionKind co
      (tv_bndrs, rho) = tcSplitForAllVarBndrs ty
      (arg_tys, io_res_ty) = tcSplitFunTys rho
  args <- newSysLocalsDs arg_tys
  (val_args, arg_wrappers) <- mapAndUnzipM asteriusUnboxArg (map Var args)
  let work_arg_ids = [v | Var v <- val_args]
  (ccall_result_ty, res_wrapper) <- asteriusBoxResult io_res_ty
  ccall_uniq <- newUnique
  work_uniq <- newUnique
  dflags <- getDynFlags
  let fcall = CCall (mkCCallSpec target cconv safety io_res_ty arg_tys)
  fcall' <- case fcall of
    CCall (CCallSpec (StaticTarget _ cName mUnitId _) CApiConv safety _ _) -> do
      wrapperName <- mkWrapperName "ghc_wrapper" (unpackFS cName)
      let fcall' =
            CCall
              ( mkCCallSpec
                  (StaticTarget NoSourceText wrapperName mUnitId True)
                  CApiConv
                  safety
                  io_res_ty
                  arg_tys
              )
      return fcall'
    _ -> return fcall
  let worker_ty =
        mkForAllTys tv_bndrs (mkVisFunTys (map idType work_arg_ids) ccall_result_ty)
      tvs = map binderVar tv_bndrs
      the_ccall_app = mkFCall dflags ccall_uniq fcall' val_args ccall_result_ty
      work_rhs = mkLams tvs (mkLams work_arg_ids the_ccall_app)
      work_id = mkSysLocal (fsLit "$wccall") work_uniq worker_ty
      work_app = mkApps (mkVarApps (Var work_id) tvs) val_args
      wrapper_body = foldr ($) (res_wrapper work_app) arg_wrappers
      wrap_rhs = mkLams (tvs ++ args) wrapper_body
      wrap_rhs' = Cast wrap_rhs co
      fn_id_w_inl =
        fn_id `setIdUnfolding` mkInlineUnfoldingWithArity (length args) wrap_rhs'
  return [(work_id, work_rhs), (fn_id_w_inl, wrap_rhs')]

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
        mkLams tvs $ Lam cback $
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
    Just FFIFunctionType {..} = parseFFIFunctionType False arg_ty
    ffi_params_tag = encodeTys ffiParamTypes
    ffi_ret_tag = encodeTys ffiResultTypes

isAnyTy :: Type -> Bool
isAnyTy ty = case tcSplitTyConApp_maybe ty of
  Just (tc, _) -> anyTyConKey == getUnique tc
  Nothing -> False

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
    return (realWorldStatePrimTy `mkVisFunTy` ccall_res_ty, wrap)
asteriusBoxResult result_ty = do
  res <- asteriusResultWrapper result_ty
  (ccall_res_ty, the_alt) <- mk_alt return_result res
  let wrap the_call =
        mkWildCase
          (App the_call (Var realWorldPrimId))
          ccall_res_ty
          (coreAltType the_alt)
          [the_alt]
  return (realWorldStatePrimTy `mkVisFunTy` ccall_res_ty, wrap)
  where
    return_result _ [ans] = ans
    return_result _ _ = panic "return_result: expected single result"

asteriusResultWrapper :: Type -> DsM (Maybe Type, CoreExpr -> CoreExpr)
asteriusResultWrapper result_ty
  | isAnyTy result_ty = return (Just result_ty, id)
  | otherwise = resultWrapper result_ty
