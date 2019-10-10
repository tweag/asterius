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
import CmmExpr
import CmmUtils
import Control.Monad
import CoreUnfold
import Data.List
import Data.Maybe
import DsCCall
import DsMonad
import DsUtils
import ErrUtils
import ForeignCall
import qualified GHC.LanguageExtensions as LangExt
import GhcPlugins
import HsSyn
import MkId
import OrdList
import Outputable
import Pair
import Platform
import PrelNames
import RepType
import TcEnv
import TcExpr
import TcForeign
import TcHsType
import TcRnMonad
import TcType
import TysPrim
import Prelude hiding ((<>))

type Binding = (Id, CoreExpr)

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
  (_, foRhs) <- resultWrapper ty
  let rhs = foRhs (Lit (MachLabel cid stdcall_info fod))
      rhs' = Cast rhs co
      stdcall_info = funTypeArgStdcallInfo dflags cconv ty
   in return [(id, rhs')]
asteriusDsCImport id co (CFunction target) cconv@PrimCallConv safety _ =
  asteriusDsPrimCall id co (CCall (CCallSpec target cconv safety))
asteriusDsCImport id co (CFunction target) cconv safety _ =
  asteriusDsFCall id co (CCall (CCallSpec target cconv safety))
asteriusDsCImport _ _ _ _ _ _ = panic "asteriusDsCImport"

funTypeArgStdcallInfo :: DynFlags -> CCallConv -> Type -> Maybe Int
funTypeArgStdcallInfo dflags StdCallConv ty
  | Just (tc, [arg_ty]) <- splitTyConApp_maybe ty,
    tyConUnique tc == funPtrTyConKey =
    let (bndrs, _) = tcSplitPiTys arg_ty
        fe_arg_tys = mapMaybe binderRelevantType_maybe bndrs
     in Just $
          sum
            ( map
                (widthInBytes . typeWidth . typeCmmType dflags . getPrimTyOf)
                fe_arg_tys
            )
funTypeArgStdcallInfo _ _other_conv _ = Nothing

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

getPrimTyOf :: Type -> UnaryType
getPrimTyOf ty
  | isBoolTy rep_ty = intPrimTy
  | otherwise =
    case splitDataProductType_maybe rep_ty of
      Just (_, _, _, [prim_ty]) -> prim_ty
      _other -> pprPanic "DsForeign.getPrimTyOf" (ppr ty)
  where
    rep_ty = unwrapType ty

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
        imp_decl'' <-
          processFFIImport globalFFIHookState sig_ty norm_sig_ty imp_decl'
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
        checkForeignArgs (isFFIArgumentTy dflags safety) arg_tys
        checkForeignRes nonIOok checkSafe (isFFIImportResultTy dflags) res_ty
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
    checkForeignArgs (isFFIArgumentTy dflags safety) arg_tys
    checkForeignRes nonIOok checkSafe (isFFIImportResultTy dflags) res_ty
    checkMissingAmpersand dflags arg_tys res_ty
    case target of
      StaticTarget _ _ _ False
        | not (null arg_tys) ->
          addErrTc (text "`value' imports cannot have function types")
      _ -> return ()
    return $ CImport (L lc cconv') (L ls safety) mh (CFunction target) src
asteriusTcCheckFIType _ _ _ = panic "asteriusTcCheckFIType"

checkMissingAmpersand :: DynFlags -> [Type] -> Type -> TcM ()
checkMissingAmpersand dflags arg_tys res_ty
  | null arg_tys && isFunPtrTy res_ty && wopt Opt_WarnDodgyForeignImports dflags =
    addWarn
      (Reason Opt_WarnDodgyForeignImports)
      (text "possible missing & in foreign import of FunPtr")
  | otherwise = return ()

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
      spec'' <- processFFIExport globalFFIHookState sig_ty norm_sig_ty id spec'
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
  checkForeignArgs isFFIExternalTy arg_tys
  checkForeignRes nonIOok noCheckSafe isFFIExportResultTy res_ty
  return (CExport (L l (CExportStatic esrc str cconv')) src)
  where
    (bndrs, res_ty) = tcSplitPiTys sig_ty
    arg_tys = mapMaybe binderRelevantType_maybe bndrs

asteriusUnboxArg :: CoreExpr -> DsM (CoreExpr, CoreExpr -> CoreExpr)
asteriusUnboxArg arg
  | isPrimitiveType arg_ty = return (arg, id)
  | Just (co, _rep_ty) <- topNormaliseNewType_maybe arg_ty =
    asteriusUnboxArg (mkCastDs arg co)
  | Just tc <- tyConAppTyCon_maybe arg_ty,
    tc `hasKey` boolTyConKey = do
    dflags <- getDynFlags
    prim_arg <- newSysLocalDs intPrimTy
    return
      ( Var prim_arg,
        \body ->
          Case
            ( mkWildCase
                arg
                arg_ty
                intPrimTy
                [ (DataAlt falseDataCon, [], mkIntLit dflags 0),
                  (DataAlt trueDataCon, [], mkIntLit dflags 1)
                ]
            )
            prim_arg
            (exprType body)
            [(DEFAULT, [], body)]
      )
  | is_product_type && data_con_arity == 1 = do
    case_bndr <- newSysLocalDs arg_ty
    prim_arg <- newSysLocalDs data_con_arg_ty1
    return
      ( Var prim_arg,
        \body ->
          Case
            arg
            case_bndr
            (exprType body)
            [(DataAlt data_con, [prim_arg], body)]
      )
  | is_product_type
      && data_con_arity == 3
      && isJust maybe_arg3_tycon
      && ( arg3_tycon == byteArrayPrimTyCon
             || arg3_tycon == mutableByteArrayPrimTyCon
         ) = do
    case_bndr <- newSysLocalDs arg_ty
    vars@[_l_var, _r_var, arr_cts_var] <- newSysLocalsDs data_con_arg_tys
    return
      ( Var arr_cts_var,
        \body ->
          Case arg case_bndr (exprType body) [(DataAlt data_con, vars, body)]
      )
  | otherwise = do
    l <- getSrcSpanDs
    pprPanic "asteriusUnboxArg: " (ppr l <+> ppr arg_ty)
  where
    arg_ty = exprType arg
    maybe_product_type = splitDataProductType_maybe arg_ty
    is_product_type = isJust maybe_product_type
    Just (_, _, data_con, data_con_arg_tys) = maybe_product_type
    data_con_arity = dataConSourceArity data_con
    (data_con_arg_ty1 : _) = data_con_arg_tys
    (_ : _ : data_con_arg_ty3 : _) = data_con_arg_tys
    maybe_arg3_tycon = tyConAppTyCon_maybe data_con_arg_ty3
    Just arg3_tycon = maybe_arg3_tycon

asteriusBoxResult :: Type -> DsM (Type, CoreExpr -> CoreExpr)
asteriusBoxResult result_ty
  | Just (io_tycon, io_res_ty) <- tcSplitIOType_maybe result_ty = do
    res <- resultWrapper io_res_ty
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
    (ccall_res_ty, the_alt) <- mkAlt return_result res
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
  res <- resultWrapper result_ty
  (ccall_res_ty, the_alt) <- mkAlt return_result res
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

mkAlt ::
  (Expr Var -> [Expr Var] -> Expr Var) ->
  (Maybe Type, Expr Var -> Expr Var) ->
  DsM (Type, (AltCon, [Id], Expr Var))
mkAlt return_result (Nothing, wrap_result) = do
  state_id <- newSysLocalDs realWorldStatePrimTy
  let the_rhs = return_result (Var state_id) [wrap_result (panic "boxResult")]
      ccall_res_ty = mkTupleTy Unboxed [realWorldStatePrimTy]
      the_alt = (DataAlt (tupleDataCon Unboxed 1), [state_id], the_rhs)
  return (ccall_res_ty, the_alt)
mkAlt return_result (Just prim_res_ty, wrap_result) = do
  result_id <- newSysLocalDs prim_res_ty
  state_id <- newSysLocalDs realWorldStatePrimTy
  let the_rhs = return_result (Var state_id) [wrap_result (Var result_id)]
      ccall_res_ty = mkTupleTy Unboxed [realWorldStatePrimTy, prim_res_ty]
      the_alt =
        (DataAlt (tupleDataCon Unboxed 2), [state_id, result_id], the_rhs)
  return (ccall_res_ty, the_alt)

checkCOrAsmOrLlvm :: HscTarget -> Validity
checkCOrAsmOrLlvm HscC = IsValid
checkCOrAsmOrLlvm HscAsm = IsValid
checkCOrAsmOrLlvm HscLlvm = IsValid
checkCOrAsmOrLlvm _ =
  NotValid
    ( text
        "requires unregisterised, llvm (-fllvm) or native code generation (-fasm)"
    )

checkCOrAsmOrLlvmOrInterp :: HscTarget -> Validity
checkCOrAsmOrLlvmOrInterp HscC = IsValid
checkCOrAsmOrLlvmOrInterp HscAsm = IsValid
checkCOrAsmOrLlvmOrInterp HscLlvm = IsValid
checkCOrAsmOrLlvmOrInterp HscInterpreted = IsValid
checkCOrAsmOrLlvmOrInterp _ =
  NotValid
    (text "requires interpreted, unregisterised, llvm or native code generation")

checkCg :: (HscTarget -> Validity) -> TcM ()
checkCg check = do
  dflags <- getDynFlags
  let target = hscTarget dflags
  case target of
    HscNothing -> return ()
    _ ->
      case check target of
        IsValid -> return ()
        NotValid err -> addErrTc (text "Illegal foreign declaration:" <+> err)

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

check :: Validity -> (MsgDoc -> MsgDoc) -> TcM ()
check IsValid _ = return ()
check (NotValid doc) err_fn = addErrTc (err_fn doc)

illegalForeignTyErr :: SDoc -> SDoc -> SDoc
illegalForeignTyErr arg_or_res = hang msg 2
  where
    msg =
      hsep
        [text "Unacceptable", arg_or_res, text "type in foreign declaration:"]

argument :: SDoc
argument = text "argument"

badCName :: CLabelString -> MsgDoc
badCName target =
  sep [quotes (ppr target) <+> text "is not a valid C identifier"]

foreignDeclCtxt :: ForeignDecl GhcRn -> SDoc
foreignDeclCtxt fo = hang (text "When checking declaration:") 2 (ppr fo)
