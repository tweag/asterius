{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Asterius.Foreign
  ( asteriusDsForeigns,
    asteriusTcForeignImports,
    asteriusTcForeignExports,
  )
where

import Asterius.Foreign.Internals
import Bag
import Control.Monad
import CoreUnfold
import Data.List
import Data.Maybe
import DsCCall
import DsForeign
import DsMonad
import ErrUtils
import ForeignCall
import qualified GHC.LanguageExtensions as LangExt
import GhcPlugins
import HsSyn
import MkId
import OrdList
import Outputable
import Pair
import Panic
import Platform
import PrelNames
import TcEnv
import TcExpr
import TcForeign
import TcHsType
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
asteriusDsFImport id co (CImport cconv safety mHeader spec _) =
  asteriusDsCImport id co spec (unLoc cconv) (unLoc safety) mHeader

asteriusDsCImport ::
  Id ->
  Coercion ->
  CImportSpec ->
  CCallConv ->
  Safety ->
  Maybe Header ->
  DsM [Binding]
asteriusDsCImport id co (CLabel cid) cconv _ _ = do
  dflags <- getDynFlags
  let ty = pFst $ coercionKind co
      fod =
        case tyConAppTyCon_maybe (dropForAlls ty) of
          Just tycon
            | tyConUnique tycon == funPtrTyConKey -> IsFunction
          _ -> IsData
  (_, foRhs) <- asteriusResultWrapper ty
  let rhs = foRhs (Lit (MachLabel cid stdcall_info fod))
      rhs' = Cast rhs co
      stdcall_info = fun_type_arg_stdcall_info dflags cconv ty
   in return [(id, rhs')]
asteriusDsCImport id co (CFunction target) cconv@PrimCallConv safety _ =
  asteriusDsPrimCall id co (CCall (CCallSpec target cconv safety))
asteriusDsCImport id co (CFunction target) cconv safety _ =
  asteriusDsFCall id co (CCall (CCallSpec target cconv safety))
asteriusDsCImport id co _ cconv safety mHeader =
  panicDoc "asteriusDsCImport" $
    vcat [ppr id, ppr co, ppr cconv, ppr safety, ppr mHeader]

asteriusDsFCall :: Id -> Coercion -> ForeignCall -> DsM [(Id, Expr TyVar)]
asteriusDsFCall fn_id co fcall = do
  let ty = pFst $ coercionKind co
      (tv_bndrs, rho) = tcSplitForAllTyVarBndrs ty
      (arg_tys, io_res_ty) = tcSplitFunTys rho
  args <- newSysLocalsDs arg_tys
  (val_args, arg_wrappers) <- mapAndUnzipM asteriusUnboxArg (map Var args)
  let work_arg_ids = [v | Var v <- val_args]
  (ccall_result_ty, res_wrapper) <- asteriusBoxResult io_res_ty
  ccall_uniq <- newUnique
  work_uniq <- newUnique
  dflags <- getDynFlags
  fcall' <-
    case fcall of
      CCall (CCallSpec (StaticTarget _ cName mUnitId _) CApiConv safety) -> do
        wrapperName <- mkWrapperName "ghc_wrapper" (unpackFS cName)
        let fcall' =
              CCall
                ( CCallSpec
                    (StaticTarget NoSourceText wrapperName mUnitId True)
                    CApiConv
                    safety
                )
        return fcall'
      _ -> return fcall
  let worker_ty =
        mkForAllTys
          tv_bndrs
          (mkFunTys (map idType work_arg_ids) ccall_result_ty)
      tvs = map binderVar tv_bndrs
      the_ccall_app = mkFCall dflags ccall_uniq fcall' val_args ccall_result_ty
      work_rhs = mkLams tvs (mkLams work_arg_ids the_ccall_app)
      work_id = mkSysLocal (fsLit "$wccall") work_uniq worker_ty
      work_app = mkApps (mkVarApps (Var work_id) tvs) val_args
      wrapper_body = foldr ($) (res_wrapper work_app) arg_wrappers
      wrap_rhs = mkLams (tvs ++ args) wrapper_body
      wrap_rhs' = Cast wrap_rhs co
      fn_id_w_inl =
        fn_id
          `setIdUnfolding` mkInlineUnfoldingWithArity (length args) wrap_rhs'
  return [(work_id, work_rhs), (fn_id_w_inl, wrap_rhs')]

asteriusDsPrimCall :: Id -> Coercion -> ForeignCall -> DsM [(Id, Expr TyVar)]
asteriusDsPrimCall fn_id co fcall = do
  let ty = pFst $ coercionKind co
      (tvs, fun_ty) = tcSplitForAllTys ty
      (arg_tys, io_res_ty) = tcSplitFunTys fun_ty
  args <- newSysLocalsDs arg_tys
  ccall_uniq <- newUnique
  dflags <- getDynFlags
  let call_app = mkFCall dflags ccall_uniq fcall (map Var args) io_res_ty
      rhs = mkLams tvs (mkLams args call_app)
      rhs' = Cast rhs co
  return [(fn_id, rhs')]

asteriusTcForeignImports ::
  [LForeignDecl GhcRn] -> TcM ([Id], [LForeignDecl GhcTc], Bag GlobalRdrElt)
asteriusTcForeignImports decls = do
  (ids, decls, gres) <-
    mapAndUnzip3M asteriusTcFImport $ filter isForeignImport decls
  return (ids, decls, unionManyBags gres)

asteriusTcFImport ::
  LForeignDecl GhcRn -> TcM (Id, LForeignDecl GhcTc, Bag GlobalRdrElt)
asteriusTcFImport
  ( L
      dloc
      fo@ForeignImport
        { fd_name = L nloc nm,
          fd_sig_ty = hs_ty,
          fd_fi = imp_decl
        }
    ) =
    setSrcSpan dloc
      $ addErrCtxt (foreignDeclCtxt fo)
      $ do
        sig_ty <- tcHsSigType (ForSigCtxt nm) hs_ty
        (norm_co, norm_sig_ty, gres) <- normaliseFfiType sig_ty
        let (bndrs, res_ty) = tcSplitPiTys norm_sig_ty
            arg_tys = mapMaybe binderRelevantType_maybe bndrs
            id = mkLocalId nm sig_ty
        imp_decl' <- asteriusTcCheckFIType arg_tys res_ty imp_decl
        imp_decl'' <- processFFIImport globalFFIHookState norm_sig_ty imp_decl'
        let fi_decl =
              ForeignImport
                { fd_name = L nloc id,
                  fd_sig_ty = undefined,
                  fd_i_ext = mkSymCo norm_co,
                  fd_fi = imp_decl''
                }
        return (id, L dloc fi_decl, gres)
asteriusTcFImport d = pprPanic "asteriusTcFImport" (ppr d)

asteriusTcCheckFIType :: [Type] -> Type -> ForeignImport -> TcM ForeignImport
asteriusTcCheckFIType arg_tys res_ty (CImport (L lc cconv) safety mh l@(CLabel _) src) = do
  checkCg checkCOrAsmOrLlvmOrInterp
  check
    (isFFILabelTy (mkFunTys arg_tys res_ty))
    (illegalForeignTyErr Outputable.empty)
  cconv' <- asteriusCheckCConv cconv
  return (CImport (L lc cconv') safety mh l src)
asteriusTcCheckFIType arg_tys res_ty idecl@(CImport (L lc cconv) (L ls safety) mh (CFunction target) src)
  | isDynamicTarget target = do
    checkCg checkCOrAsmOrLlvmOrInterp
    cconv' <- asteriusCheckCConv cconv
    case arg_tys of
      [] ->
        addErrTc
          ( illegalForeignTyErr
              Outputable.empty
              (text "At least one argument expected")
          )
      (arg1_ty : arg_tys) -> do
        dflags <- getDynFlags
        let curried_res_ty = mkFunTys arg_tys res_ty
        check (isFFIDynTy curried_res_ty arg1_ty) (illegalForeignTyErr argument)
        checkForeignArgs (asteriusIsFFIArgumentTy dflags safety) arg_tys
        checkForeignRes
          nonIOok
          checkSafe
          (asteriusIsFFIImportResultTy dflags)
          res_ty
    return $ CImport (L lc cconv') (L ls safety) mh (CFunction target) src
  | cconv == PrimCallConv = do
    dflags <- getDynFlags
    checkTc
      (xopt LangExt.GHCForeignImportPrim dflags)
      (text "Use GHCForeignImportPrim to allow `foreign import prim'.")
    checkCg checkCOrAsmOrLlvmOrInterp
    checkCTarget target
    checkTc
      (playSafe safety)
      ( text
          "The safe/unsafe annotation should not be used with `foreign import prim'."
      )
    checkForeignArgs (isFFIPrimArgumentTy dflags) arg_tys
    checkForeignRes nonIOok checkSafe (isFFIPrimResultTy dflags) res_ty
    return idecl
  | otherwise = do
    checkCg checkCOrAsmOrLlvmOrInterp
    cconv' <- asteriusCheckCConv cconv
    dflags <- getDynFlags
    checkForeignArgs (asteriusIsFFIArgumentTy dflags safety) arg_tys
    checkForeignRes
      nonIOok
      checkSafe
      (asteriusIsFFIImportResultTy dflags)
      res_ty
    checkMissingAmpersand dflags arg_tys res_ty
    case target of
      StaticTarget _ _ _ False
        | not (null arg_tys) ->
          addErrTc (text "`value' imports cannot have function types")
      _ -> return ()
    return $ CImport (L lc cconv') (L ls safety) mh (CFunction target) src
asteriusTcCheckFIType arg_tys res_ty imp_decl =
  panicDoc "asteriusTcCheckFIType" $
    vcat [ppr arg_tys, ppr res_ty, ppr imp_decl]

asteriusTcForeignExports ::
  [LForeignDecl GhcRn] ->
  TcM (LHsBinds GhcTcId, [LForeignDecl GhcTcId], Bag GlobalRdrElt)
asteriusTcForeignExports decls =
  foldlM combine (emptyLHsBinds, [], emptyBag) (filter isForeignExport decls)
  where
    combine (binds, fs, gres1) (L loc fe) = do
      (b, f, gres2) <- setSrcSpan loc (asteriusTcFExport fe)
      return (b `consBag` binds, L loc f : fs, gres1 `unionBags` gres2)

asteriusTcFExport ::
  ForeignDecl GhcRn ->
  TcM (LHsBind GhcTc, ForeignDecl GhcTc, Bag GlobalRdrElt)
asteriusTcFExport
  fo@ForeignExport
    { fd_name = L loc nm,
      fd_sig_ty = hs_ty,
      fd_fe = spec
    } =
    addErrCtxt (foreignDeclCtxt fo) $ do
      sig_ty <- tcHsSigType (ForSigCtxt nm) hs_ty
      rhs <- tcPolyExpr (nlHsVar nm) sig_ty
      (norm_co, norm_sig_ty, gres) <- normaliseFfiType sig_ty
      spec' <- asteriusTcCheckFEType norm_sig_ty spec
      id <- mkStableIdFromName nm sig_ty loc mkForeignExportOcc
      spec'' <- processFFIExport globalFFIHookState norm_sig_ty id spec'
      return
        ( mkVarBind id rhs,
          ForeignExport
            { fd_name = L loc id,
              fd_sig_ty = undefined,
              fd_e_ext = norm_co,
              fd_fe = spec''
            },
          gres
        )
asteriusTcFExport d = pprPanic "asteriusTcFExport" (ppr d)

asteriusTcCheckFEType :: Type -> ForeignExport -> TcM ForeignExport
asteriusTcCheckFEType sig_ty (CExport (L l (CExportStatic esrc str cconv)) src) = do
  checkCg checkCOrAsmOrLlvm
  checkTc (isCLabelString str) (badCName str)
  cconv' <- asteriusCheckCConv cconv
  checkForeignArgs asteriusIsFFIExternalTy arg_tys
  checkForeignRes nonIOok noCheckSafe asteriusIsFFIExportResultTy res_ty
  return (CExport (L l (CExportStatic esrc str cconv')) src)
  where
    (bndrs, res_ty) = tcSplitPiTys sig_ty
    arg_tys = mapMaybe binderRelevantType_maybe bndrs

asteriusIsFFIArgumentTy :: DynFlags -> Safety -> Type -> Validity
asteriusIsFFIArgumentTy dflags safety ty
  | isAnyTy ty = IsValid
  | isJSValTy ty = IsValid
  | otherwise = isFFIArgumentTy dflags safety ty

asteriusIsFFIImportResultTy :: DynFlags -> Type -> Validity
asteriusIsFFIImportResultTy dflags ty
  | isAnyTy ty = IsValid
  | isJSValTy ty = IsValid
  | otherwise = isFFIImportResultTy dflags ty

asteriusIsFFIExternalTy :: Type -> Validity
asteriusIsFFIExternalTy ty
  | isAnyTy ty = IsValid
  | isJSValTy ty = IsValid
  | otherwise = isFFIExternalTy ty

asteriusIsFFIExportResultTy :: Type -> Validity
asteriusIsFFIExportResultTy ty
  | isAnyTy ty = IsValid
  | isJSValTy ty = IsValid
  | otherwise = isFFIExportResultTy ty

isAnyTy :: Type -> Bool
isAnyTy ty =
  case tcSplitTyConApp_maybe ty of
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
    let extra_result_tys =
          case res of
            (Just ty, _)
              | isUnboxedTupleType ty ->
                let Just ls = tyConAppArgs_maybe ty
                 in tail ls
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

asteriusCheckCConv :: CCallConv -> TcM CCallConv
asteriusCheckCConv CCallConv = return CCallConv
asteriusCheckCConv CApiConv = return CApiConv
asteriusCheckCConv StdCallConv = do
  dflags <- getDynFlags
  let platform = targetPlatform dflags
  if platformArch platform == ArchX86
    then return StdCallConv
    else do
      when (wopt Opt_WarnUnsupportedCallingConventions dflags) $
        addWarnTc
          (Reason Opt_WarnUnsupportedCallingConventions)
          ( text
              "the 'stdcall' calling convention is unsupported on this platform,"
              $$ text "treating as ccall"
          )
      return CCallConv
asteriusCheckCConv PrimCallConv = do
  addErrTc
    (text "The `prim' calling convention can only be used with `foreign import'")
  return PrimCallConv
asteriusCheckCConv JavaScriptCallConv = return JavaScriptCallConv
