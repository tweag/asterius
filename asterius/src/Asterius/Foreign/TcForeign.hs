{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Asterius.Foreign.TcForeign
  ( asteriusTcForeignImports,
    asteriusTcForeignExports,
  )
where

import Asterius.Foreign.Internals
import Bag
import Control.Monad
import Data.List
import Data.Maybe
import DsMonad
import ErrUtils
import ForeignCall
import qualified GHC.LanguageExtensions as LangExt
import GhcPlugins
import HsSyn
import Outputable
import Platform
import PrelNames
import TcEnv
import TcExpr
import TcForeign
import TcHsType
import TcRnMonad
import TcType
import Prelude hiding ((<>))

asteriusTcForeignImports ::
  [LForeignDecl GhcRn] -> TcM ([Id], [LForeignDecl GhcTc], Bag GlobalRdrElt)
asteriusTcForeignImports decls = do
  (ids, decls, gres) <-
    mapAndUnzip3M asteriusTcFImport $
      filter isForeignImport decls
  return (ids, decls, unionManyBags gres)

asteriusTcFImport ::
  LForeignDecl GhcRn -> TcM (Id, LForeignDecl GhcTc, Bag GlobalRdrElt)
asteriusTcFImport (L dloc fo@ForeignImport {fd_name = L nloc nm, fd_sig_ty = hs_ty, fd_fi = imp_decl}) =
  setSrcSpan dloc $ addErrCtxt (foreignDeclCtxt fo) $ do
    sig_ty <- tcHsSigType (ForSigCtxt nm) hs_ty
    (norm_co, norm_sig_ty, gres) <- normaliseFfiType sig_ty
    let (bndrs, res_ty) = tcSplitPiTys norm_sig_ty
        arg_tys = mapMaybe binderRelevantType_maybe bndrs
        id = mkLocalId nm sig_ty
    imp_decl' <- asteriusTcCheckFIType arg_tys res_ty imp_decl
    imp_decl'' <- processFFIImport globalFFIHookState norm_sig_ty imp_decl'
    let fi_decl = ForeignImport
          { fd_name = L nloc id,
            fd_sig_ty = undefined,
            fd_i_ext = mkSymCo norm_co,
            fd_fi = imp_decl''
          }
    return (id, L dloc fi_decl, gres)
asteriusTcFImport d = pprPanic "asteriusTcFImport" (ppr d)

asteriusTcCheckFIType :: [Type] -> Type -> ForeignImport -> TcM ForeignImport
asteriusTcCheckFIType arg_tys res_ty (CImport (L lc cconv) safety mh l@(CLabel _) src) =
  do
    checkCg checkCOrAsmOrLlvmOrInterp
    check
      (isFFILabelTy (mkFunTys arg_tys res_ty))
      (illegalForeignTyErr Outputable.empty)
    cconv' <- asteriusCheckCConv cconv
    return (CImport (L lc cconv') safety mh l src)
asteriusTcCheckFIType arg_tys res_ty (CImport (L lc cconv) safety mh CWrapper src) =
  do
    checkCg checkCOrAsmOrLlvmOrInterp
    cconv' <- asteriusCheckCConv cconv
    case arg_tys of
      [arg1_ty] -> do
        checkForeignArgs asteriusIsFFIExternalTy arg1_tys
        checkForeignRes nonIOok checkSafe asteriusIsFFIExportResultTy res1_ty
        checkForeignRes mustBeIO checkSafe (isFFIDynTy arg1_ty) res_ty
        where
          (arg1_tys, res1_ty) = tcSplitFunTys arg1_ty
      _ ->
        addErrTc
          (illegalForeignTyErr Outputable.empty (text "One argument expected"))
    return (CImport (L lc cconv') safety mh CWrapper src)
asteriusTcCheckFIType arg_tys res_ty idecl@(CImport (L lc cconv) (L ls safety) mh (CFunction target) src)
  | isDynamicTarget target =
    do
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

asteriusTcForeignExports ::
  [LForeignDecl GhcRn] ->
  TcM (LHsBinds GhcTcId, [LForeignDecl GhcTcId], Bag GlobalRdrElt)
asteriusTcForeignExports decls =
  foldlM
    combine
    (emptyLHsBinds, [], emptyBag)
    (filter isForeignExport decls)
  where
    combine (binds, fs, gres1) (L loc fe) = do
      (b, f, gres2) <- setSrcSpan loc (asteriusTcFExport fe)
      return (b `consBag` binds, L loc f : fs, gres1 `unionBags` gres2)

asteriusTcFExport ::
  ForeignDecl GhcRn ->
  TcM (LHsBind GhcTc, ForeignDecl GhcTc, Bag GlobalRdrElt)
asteriusTcFExport fo@ForeignExport {fd_name = L loc nm, fd_sig_ty = hs_ty, fd_fe = spec} =
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
asteriusTcCheckFEType sig_ty (CExport (L l (CExportStatic esrc str cconv)) src) =
  do
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
isAnyTy ty = case tcSplitTyConApp_maybe ty of
  Just (tc, _) -> anyTyConKey == getUnique tc
  Nothing -> False

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
    ( text "The `prim' calling convention can only be used with `foreign import'"
    )
  return PrimCallConv
asteriusCheckCConv JavaScriptCallConv = return JavaScriptCallConv
