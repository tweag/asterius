{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Asterius.Foreign
  ( asteriusDsForeigns,
    asteriusTcForeignImports,
    asteriusTcForeignExports
    )
where

import Bag
import CmmExpr
import CmmUtils
import Control.Monad
import CoreUnfold
import Data.List
import Data.Maybe
import DsCCall
import DsMonad
import ErrUtils
import FamInst
import FamInstEnv
import ForeignCall
import qualified GHC.LanguageExtensions as LangExt
import GhcPlugins
import HsSyn
import OrdList
import Outputable
import Pair
import Platform
import PrelNames
import RepType
import TcEnv
import TcExpr
import TcHsType
import TcRnMonad
import TcType
import TysPrim
import Prelude hiding ((<>))

type Binding = (Id, CoreExpr)

asteriusDsForeigns
  :: [LForeignDecl GhcTc] -> DsM (ForeignStubs, OrdList Binding)
asteriusDsForeigns [] = return (NoStubs, nilOL)
asteriusDsForeigns fos = do
  bindss <- mapM do_ldecl fos
  return (NoStubs, foldr (appOL . toOL) nilOL bindss)
  where
    do_ldecl (L loc decl) = putSrcSpanDs loc (do_decl decl)
    do_decl ForeignImport {fd_name = id, fd_i_ext = co, fd_fi = spec} = do
      traceIf (text "fi start" <+> ppr id)
      let id' = unLoc id
      (bs, _, _) <- asteriusDsFImport id' co spec
      traceIf (text "fi end" <+> ppr id)
      return bs
    do_decl
      ForeignExport
        { fd_name = L _ _,
          fd_e_ext = _,
          fd_fe = CExport (L _ CExportStatic {}) _
          } = return []
    do_decl (XForeignDecl _) = panic "asteriusDsForeigns"

asteriusDsFImport
  :: Id -> Coercion -> ForeignImport -> DsM ([Binding], SDoc, SDoc)
asteriusDsFImport id co (CImport cconv safety mHeader spec _) =
  asteriusDsCImport id co spec (unLoc cconv) (unLoc safety) mHeader

asteriusDsCImport
  :: Id
  -> Coercion
  -> CImportSpec
  -> CCallConv
  -> Safety
  -> Maybe Header
  -> DsM ([Binding], SDoc, SDoc)
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
   in return ([(id, rhs')], empty, empty)
asteriusDsCImport id co (CFunction target) cconv@PrimCallConv safety _ =
  dsPrimCall id co (CCall (CCallSpec target cconv safety))
asteriusDsCImport id co (CFunction target) cconv safety mHeader =
  dsFCall id co (CCall (CCallSpec target cconv safety)) mHeader
asteriusDsCImport _ _ _ _ _ _ = panic "asteriusDsCImport"

funTypeArgStdcallInfo :: DynFlags -> CCallConv -> Type -> Maybe Int
funTypeArgStdcallInfo dflags StdCallConv ty
  | Just (tc, [arg_ty]) <- splitTyConApp_maybe ty,
    tyConUnique tc == funPtrTyConKey =
    let (bndrs, _) = tcSplitPiTys arg_ty
        fe_arg_tys = mapMaybe binderRelevantType_maybe bndrs
     in Just
          $ sum
              ( map
                  (widthInBytes . typeWidth . typeCmmType dflags . getPrimTyOf)
                  fe_arg_tys
                )
funTypeArgStdcallInfo _ _other_conv _ = Nothing

dsFCall
  :: Id
  -> Coercion
  -> ForeignCall
  -> Maybe Header
  -> DsM ([(Id, Expr TyVar)], SDoc, SDoc)
dsFCall fn_id co fcall mDeclHeader = do
  let ty = pFst $ coercionKind co
      (tv_bndrs, rho) = tcSplitForAllTyVarBndrs ty
      (arg_tys, io_res_ty) = tcSplitFunTys rho
  args <- newSysLocalsDs arg_tys
  (val_args, arg_wrappers) <- mapAndUnzipM unboxArg (map Var args)
  let work_arg_ids = [v | Var v <- val_args]
  (ccall_result_ty, res_wrapper) <- boxResult io_res_ty
  ccall_uniq <- newUnique
  work_uniq <- newUnique
  dflags <- getDynFlags
  (fcall', cDoc) <-
    case fcall of
      CCall (CCallSpec (StaticTarget _ cName mUnitId isFun) CApiConv safety) -> do
        wrapperName <- mkWrapperName "ghc_wrapper" (unpackFS cName)
        let fcall' =
              CCall
                ( CCallSpec
                    (StaticTarget NoSourceText wrapperName mUnitId True)
                    CApiConv
                    safety
                  )
            c = includes $$ fun_proto <+> braces (cRet <> semi)
            includes =
              vcat
                [ text "#include \"" <> ftext h <> text "\""
                  | Header _ h <- nub headers
                  ]
            fun_proto =
              cResType <+> pprCconv <+> ppr wrapperName <> parens argTypes
            cRet
              | isVoidRes = cCall
              | otherwise = text "return" <+> cCall
            cCall
              | isFun = ppr cName <> parens argVals
              | null arg_tys = ppr cName
              | otherwise =
                panic "dsFCall: Unexpected arguments to FFI value import"
            raw_res_ty =
              case tcSplitIOType_maybe io_res_ty of
                Just (_ioTyCon, res_ty) -> res_ty
                Nothing -> io_res_ty
            isVoidRes = raw_res_ty `eqType` unitTy
            (mHeader, cResType)
              | isVoidRes = (Nothing, text "void")
              | otherwise = toCType raw_res_ty
            pprCconv = ccallConvAttribute CApiConv
            mHeadersArgTypeList =
              [ (header, cType <+> char 'a' <> int n)
                | (t, n) <- zip arg_tys [1 ..],
                  let (header, cType) = toCType t
                ]
            (mHeaders, argTypeList) = unzip mHeadersArgTypeList
            argTypes =
              if null argTypeList
              then text "void"
              else hsep $ punctuate comma argTypeList
            mHeaders' = mDeclHeader : mHeader : mHeaders
            headers = catMaybes mHeaders'
            argVals =
              hsep
                $ punctuate comma [char 'a' <> int n | (_, n) <- zip arg_tys [1 ..]]
        return (fcall', c)
      _ -> return (fcall, empty)
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
  return ([(work_id, work_rhs), (fn_id_w_inl, wrap_rhs')], empty, cDoc)

dsPrimCall
  :: Id -> Coercion -> ForeignCall -> DsM ([(Id, Expr TyVar)], SDoc, SDoc)
dsPrimCall fn_id co fcall = do
  let ty = pFst $ coercionKind co
      (tvs, fun_ty) = tcSplitForAllTys ty
      (arg_tys, io_res_ty) = tcSplitFunTys fun_ty
  args <- newSysLocalsDs arg_tys
  ccall_uniq <- newUnique
  dflags <- getDynFlags
  let call_app = mkFCall dflags ccall_uniq fcall (map Var args) io_res_ty
      rhs = mkLams tvs (mkLams args call_app)
      rhs' = Cast rhs co
  return ([(fn_id, rhs')], empty, empty)

toCType :: Type -> (Maybe Header, SDoc)
toCType = f False
  where
    f voidOK t
      | Just (ptr, [t']) <- splitTyConApp_maybe t,
        tyConName ptr `elem` [ptrTyConName, funPtrTyConName] =
        case f True t' of
          (mh, cType') -> (mh, cType' <> char '*')
      | Just tycon <- tyConAppTyConPicky_maybe t,
        Just (CType _ mHeader (_, cType)) <- tyConCType_maybe tycon =
        (mHeader, ftext cType)
      | Just t' <- coreView t = f voidOK t'
      | Just byteArrayPrimTyCon == tyConAppTyConPicky_maybe t =
        (Nothing, text "const void*")
      | Just mutableByteArrayPrimTyCon == tyConAppTyConPicky_maybe t =
        (Nothing, text "void*")
      | voidOK = (Nothing, text "void")
      | otherwise = pprPanic "toCType" (ppr t)

getPrimTyOf :: Type -> UnaryType
getPrimTyOf ty
  | isBoolTy rep_ty = intPrimTy
  | otherwise =
    case splitDataProductType_maybe rep_ty of
      Just (_, _, _, [prim_ty]) -> prim_ty
      _other -> pprPanic "DsForeign.getPrimTyOf" (ppr ty)
  where
    rep_ty = unwrapType ty

isForeignImport :: LForeignDecl name -> Bool
isForeignImport (L _ ForeignImport {}) = True
isForeignImport _ = False

isForeignExport :: LForeignDecl name -> Bool
isForeignExport (L _ ForeignExport {}) = True
isForeignExport _ = False

normaliseFfiType :: Type -> TcM (Coercion, Type, Bag GlobalRdrElt)
normaliseFfiType ty = do
  fam_envs <- tcGetFamInstEnvs
  normaliseFfiType' fam_envs ty

normaliseFfiType'
  :: FamInstEnvs -> Type -> TcM (Coercion, Type, Bag GlobalRdrElt)
normaliseFfiType' env = go initRecTc
  where
    go :: RecTcChecker -> Type -> TcM (Coercion, Type, Bag GlobalRdrElt)
    go rec_nts ty
      | Just ty' <- tcView ty = go rec_nts ty'
      | Just (tc, tys) <- splitTyConApp_maybe ty = go_tc_app rec_nts tc tys
      | (bndrs, inner_ty) <- splitForAllTyVarBndrs ty,
        not (null bndrs) =
        do
          (coi, nty1, gres1) <- go rec_nts inner_ty
          return
            ( mkHomoForAllCos (binderVars bndrs) coi,
              mkForAllTys bndrs nty1,
              gres1
              )
      | otherwise = return (mkRepReflCo ty, ty, emptyBag)
    go_tc_app
      :: RecTcChecker
      -> TyCon
      -> [Type]
      -> TcM (Coercion, Type, Bag GlobalRdrElt)
    go_tc_app rec_nts tc tys
      | tc_key `elem` [ioTyConKey, funPtrTyConKey, funTyConKey] = children_only
      | isNewTyCon tc,
        Just rec_nts' <- checkRecTc rec_nts tc =
        do
          rdr_env <- getGlobalRdrEnv
          case checkNewtypeFFI rdr_env tc of
            Nothing -> nothing
            Just gre -> do
              (co', ty', gres) <- go rec_nts' nt_rhs
              return (mkTransCo nt_co co', ty', gre `consBag` gres)
      | isFamilyTyCon tc,
        (co, ty) <- normaliseTcApp env Representational tc tys,
        not (isReflexiveCo co) =
        do
          (co', ty', gres) <- go rec_nts ty
          return (mkTransCo co co', ty', gres)
      | otherwise = nothing
      where
        tc_key = getUnique tc
        children_only = do
          xs <- mapM (go rec_nts) tys
          let (cos, tys', gres) = unzip3 xs
              cos' =
                zipWith3
                  downgradeRole
                  (tyConRoles tc)
                  (repeat Representational)
                  cos
          return
            ( mkTyConAppCo Representational tc cos',
              mkTyConApp tc tys',
              unionManyBags gres
              )
        nt_co = mkUnbranchedAxInstCo Representational (newTyConCo tc) tys []
        nt_rhs = newTyConInstRhs tc tys
        ty = mkTyConApp tc tys
        nothing = return (mkRepReflCo ty, ty, emptyBag)

checkNewtypeFFI :: GlobalRdrEnv -> TyCon -> Maybe GlobalRdrElt
checkNewtypeFFI rdr_env tc
  | Just con <- tyConSingleDataCon_maybe tc,
    Just gre <- lookupGRE_Name rdr_env (dataConName con) =
    Just gre
  | otherwise = Nothing

asteriusTcForeignImports
  :: [LForeignDecl GhcRn] -> TcM ([Id], [LForeignDecl GhcTc], Bag GlobalRdrElt)
asteriusTcForeignImports decls = do
  (ids, decls, gres) <-
    mapAndUnzip3M asteriusTcFImport $ filter isForeignImport decls
  return (ids, decls, unionManyBags gres)

asteriusTcFImport
  :: LForeignDecl GhcRn -> TcM (Id, LForeignDecl GhcTc, Bag GlobalRdrElt)
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
      $ addErrCtxt (foreignDeclCtxt fo) $ do
      sig_ty <- tcHsSigType (ForSigCtxt nm) hs_ty
      (norm_co, norm_sig_ty, gres) <- normaliseFfiType sig_ty
      let (bndrs, res_ty) = tcSplitPiTys norm_sig_ty
          arg_tys = mapMaybe binderRelevantType_maybe bndrs
          id = mkLocalId nm sig_ty
      imp_decl' <- asteriusTcCheckFIType arg_tys res_ty imp_decl
      let fi_decl =
            ForeignImport
              { fd_name = L nloc id,
                fd_sig_ty = undefined,
                fd_i_ext = mkSymCo norm_co,
                fd_fi = imp_decl'
                }
      return (id, L dloc fi_decl, gres)
asteriusTcFImport d = pprPanic "asteriusTcFImport" (ppr d)

asteriusTcCheckFIType :: [Type] -> Type -> ForeignImport -> TcM ForeignImport
asteriusTcCheckFIType arg_tys res_ty (CImport (L lc cconv) safety mh l@(CLabel _) src) = do
  checkCg checkCOrAsmOrLlvmOrInterp
  check
    (isFFILabelTy (mkFunTys arg_tys res_ty))
    (illegalForeignTyErr Outputable.empty)
  cconv' <- checkCConv cconv
  return (CImport (L lc cconv') safety mh l src)
asteriusTcCheckFIType arg_tys res_ty idecl@(CImport (L lc cconv) (L ls safety) mh (CFunction target) src)
  | isDynamicTarget target =
    do
      checkCg checkCOrAsmOrLlvmOrInterp
      cconv' <- checkCConv cconv
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
  | cconv == PrimCallConv =
    do
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
  | otherwise =
    do
      checkCg checkCOrAsmOrLlvmOrInterp
      cconv' <- checkCConv cconv
      checkCTarget target
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

checkCTarget :: CCallTarget -> TcM ()
checkCTarget (StaticTarget _ str _ _) = do
  checkCg checkCOrAsmOrLlvmOrInterp
  checkTc (isCLabelString str) (badCName str)
checkCTarget DynamicTarget = panic "checkCTarget DynamicTarget"

checkMissingAmpersand :: DynFlags -> [Type] -> Type -> TcM ()
checkMissingAmpersand dflags arg_tys res_ty
  | null arg_tys && isFunPtrTy res_ty && wopt Opt_WarnDodgyForeignImports dflags =
    addWarn
      (Reason Opt_WarnDodgyForeignImports)
      (text "possible missing & in foreign import of FunPtr")
  | otherwise = return ()

asteriusTcForeignExports
  :: [LForeignDecl GhcRn]
  -> TcM (LHsBinds GhcTcId, [LForeignDecl GhcTcId], Bag GlobalRdrElt)
asteriusTcForeignExports decls =
  foldlM combine (emptyLHsBinds, [], emptyBag) (filter isForeignExport decls)
  where
    combine (binds, fs, gres1) (L loc fe) = do
      (b, f, gres2) <- setSrcSpan loc (tcFExport fe)
      return (b `consBag` binds, L loc f : fs, gres1 `unionBags` gres2)

tcFExport
  :: ForeignDecl GhcRn
  -> TcM (LHsBind GhcTc, ForeignDecl GhcTc, Bag GlobalRdrElt)
tcFExport fo@ForeignExport {fd_name = L loc nm, fd_sig_ty = hs_ty, fd_fe = spec} =
  addErrCtxt (foreignDeclCtxt fo) $ do
    sig_ty <- tcHsSigType (ForSigCtxt nm) hs_ty
    rhs <- tcPolyExpr (nlHsVar nm) sig_ty
    (norm_co, norm_sig_ty, gres) <- normaliseFfiType sig_ty
    spec' <- tcCheckFEType norm_sig_ty spec
    id <- mkStableIdFromName nm sig_ty loc mkForeignExportOcc
    return
      ( mkVarBind id rhs,
        ForeignExport
          { fd_name = L loc id,
            fd_sig_ty = undefined,
            fd_e_ext = norm_co,
            fd_fe = spec'
            },
        gres
        )
tcFExport d = pprPanic "tcFExport" (ppr d)

tcCheckFEType :: Type -> ForeignExport -> TcM ForeignExport
tcCheckFEType sig_ty (CExport (L l (CExportStatic esrc str cconv)) src) = do
  checkCg checkCOrAsmOrLlvm
  checkTc (isCLabelString str) (badCName str)
  cconv' <- checkCConv cconv
  checkForeignArgs isFFIExternalTy arg_tys
  checkForeignRes nonIOok noCheckSafe isFFIExportResultTy res_ty
  return (CExport (L l (CExportStatic esrc str cconv')) src)
  where
    (bndrs, res_ty) = tcSplitPiTys sig_ty
    arg_tys = mapMaybe binderRelevantType_maybe bndrs

checkForeignArgs :: (Type -> Validity) -> [Type] -> TcM ()
checkForeignArgs pred = mapM_ go
  where
    go ty = check (pred ty) (illegalForeignTyErr argument)

checkForeignRes :: Bool -> Bool -> (Type -> Validity) -> Type -> TcM ()
checkForeignRes non_io_result_ok check_safe pred_res_ty ty
  | Just (_, res_ty) <- tcSplitIOType_maybe ty =
    check (pred_res_ty res_ty) (illegalForeignTyErr result)
  | not non_io_result_ok =
    addErrTc $ illegalForeignTyErr result (text "IO result type expected")
  | otherwise =
    do
      dflags <- getDynFlags
      case pred_res_ty ty of
        NotValid msg -> addErrTc $ illegalForeignTyErr result msg
        _
          | check_safe && safeInferOn dflags -> recordUnsafeInfer emptyBag
        _
          | check_safe && safeLanguageOn dflags ->
            addErrTc (illegalForeignTyErr result safeHsErr)
        _ -> return ()
  where
    safeHsErr =
      text "Safe Haskell is on, all FFI imports must be in the IO monad"

nonIOok :: Bool
nonIOok = True

checkSafe, noCheckSafe :: Bool
checkSafe = True

noCheckSafe = False

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

checkCConv :: CCallConv -> TcM CCallConv
checkCConv CCallConv = return CCallConv
checkCConv CApiConv = return CApiConv
checkCConv StdCallConv = do
  dflags <- getDynFlags
  let platform = targetPlatform dflags
  if platformArch platform == ArchX86
  then return StdCallConv
  else
    do
      when (wopt Opt_WarnUnsupportedCallingConventions dflags)
        $ addWarnTc
            (Reason Opt_WarnUnsupportedCallingConventions)
            ( text
                "the 'stdcall' calling convention is unsupported on this platform,"
                $$ text "treating as ccall"
              )
      return CCallConv
checkCConv PrimCallConv = do
  addErrTc
    (text "The `prim' calling convention can only be used with `foreign import'")
  return PrimCallConv
checkCConv JavaScriptCallConv = do
  dflags <- getDynFlags
  if platformArch (targetPlatform dflags) == ArchJavaScript
  then return JavaScriptCallConv
  else
    do
      addErrTc
        ( text
            "The `javascript' calling convention is unsupported on this platform"
          )
      return JavaScriptCallConv

check :: Validity -> (MsgDoc -> MsgDoc) -> TcM ()
check IsValid _ = return ()
check (NotValid doc) err_fn = addErrTc (err_fn doc)

illegalForeignTyErr :: SDoc -> SDoc -> SDoc
illegalForeignTyErr arg_or_res = hang msg 2
  where
    msg =
      hsep
        [text "Unacceptable", arg_or_res, text "type in foreign declaration:"]

argument, result :: SDoc
argument = text "argument"

result = text "result"

badCName :: CLabelString -> MsgDoc
badCName target =
  sep [quotes (ppr target) <+> text "is not a valid C identifier"]

foreignDeclCtxt :: ForeignDecl GhcRn -> SDoc
foreignDeclCtxt fo = hang (text "When checking declaration:") 2 (ppr fo)
