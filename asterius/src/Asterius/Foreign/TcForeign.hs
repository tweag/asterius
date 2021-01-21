{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Asterius.Foreign.TcForeign
  ( asteriusTcForeignImports,
    asteriusTcForeignExports,
  )
where

import Asterius.Foreign.Internals
import Asterius.Foreign.SupportedTypes
import Bag
import Control.Monad
import Data.List
import Data.Maybe
import DsMonad
import ErrUtils
import ForeignCall
import GhcPlugins
import HsSyn
import Outputable
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
asteriusTcCheckFIType arg_tys res_ty (CImport (L lc JavaScriptCallConv) (L ls safety) mh (CFunction target) src)
  | unLoc src
      `elem` map
        SourceText
        ["\"wrapper\"", "\"wrapper oneshot\""] =
    do
      case arg_tys of
        [arg1_ty] -> do
          checkForeignArgs asteriusIsFFIExternalTy arg1_tys
          checkForeignRes nonIOok checkSafe asteriusIsFFIExportResultTy res1_ty
          checkForeignRes
            mustBeIO
            checkSafe
            ( \ty ->
                if isJSValTy ty
                  then IsValid
                  else
                    NotValid
                      ( text
                          "foreign import javascript \"wrapper\" expects a JSVal result"
                      )
            )
            res_ty
          where
            (arg1_tys, res1_ty) = tcSplitFunTys arg1_ty
        _ ->
          addErrTc
            (illegalForeignTyErr Outputable.empty (text "One argument expected"))
      return (CImport (L lc JavaScriptCallConv) (L ls safety) mh CWrapper src)
  | otherwise =
    do
      dflags <- getDynFlags
      checkForeignArgs (asteriusIsFFIArgumentTy dflags safety) arg_tys
      checkForeignRes
        nonIOok
        checkSafe
        (asteriusIsFFIImportResultTy dflags)
        res_ty
      return $ CImport (L lc JavaScriptCallConv) (L ls safety) mh (CFunction target) src
asteriusTcCheckFIType arg_tys res_ty imp_decl =
  tcCheckFIType arg_tys res_ty imp_decl

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
asteriusTcCheckFEType sig_ty (CExport (L l (CExportStatic esrc str JavaScriptCallConv)) src) =
  do
    checkCg checkCOrAsmOrLlvm
    checkTc (isCLabelString str) (badCName str)
    checkForeignArgs asteriusIsFFIExternalTy arg_tys
    checkForeignRes nonIOok noCheckSafe asteriusIsFFIExportResultTy res_ty
    return (CExport (L l (CExportStatic esrc str JavaScriptCallConv)) src)
  where
    (bndrs, res_ty) = tcSplitPiTys sig_ty
    arg_tys = mapMaybe binderRelevantType_maybe bndrs
asteriusTcCheckFEType norm_sig_ty spec = tcCheckFEType norm_sig_ty spec

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
