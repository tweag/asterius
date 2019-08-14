{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Asterius.Foreign
  ( asteriusDsForeigns
    )
where

import BasicTypes
import CmmExpr
import CmmUtils
import Coercion
import Config
import CoreSyn
import CoreUnfold
import Data.List
import Data.Maybe
import DataCon
import DsCCall
import DsMonad
import DynFlags
import Encoding
import FastString
import ForeignCall
import HsSyn
import HscTypes
import Id
import Literal
import Module
import Name
import OrdList
import Outputable
import Pair
import Platform
import PrelNames
import RepType
import SrcLoc
import TcEnv
import TcRnMonad
import TcType
import TyCon
import Type
import TysPrim
import TysWiredIn
import Prelude hiding ((<>))

type Binding = (Id, CoreExpr)

asteriusDsForeigns
  :: [LForeignDecl GhcTc] -> DsM (ForeignStubs, OrdList Binding)
asteriusDsForeigns [] = return (NoStubs, nilOL)
asteriusDsForeigns fos = do
  fives <- mapM do_ldecl fos
  let (hs, cs, idss, bindss) = unzip4 fives
      fe_ids = concat idss
      fe_init_code = map foreignExportInitialiser fe_ids
  return
    ( ForeignStubs (vcat hs) (vcat cs $$ vcat fe_init_code),
      foldr (appOL . toOL) nilOL bindss
      )
  where
    do_ldecl (L loc decl) = putSrcSpanDs loc (do_decl decl)
    do_decl ForeignImport {fd_name = id, fd_i_ext = co, fd_fi = spec} = do
      traceIf (text "fi start" <+> ppr id)
      let id' = unLoc id
      (bs, h, c) <- dsFImport id' co spec
      traceIf (text "fi end" <+> ppr id)
      return (h, c, [], bs)
    do_decl
      ForeignExport
        { fd_name = L _ id,
          fd_e_ext = co,
          fd_fe = CExport (L _ (CExportStatic _ ext_nm cconv)) _
          } = do
        (h, c, _, _) <- dsFExport id co ext_nm cconv False
        return (h, c, [id], [])
    do_decl (XForeignDecl _) = panic "asteriusDsForeigns"

dsFImport :: Id -> Coercion -> ForeignImport -> DsM ([Binding], SDoc, SDoc)
dsFImport id co (CImport cconv safety mHeader spec _) =
  dsCImport id co spec (unLoc cconv) (unLoc safety) mHeader

dsCImport
  :: Id
  -> Coercion
  -> CImportSpec
  -> CCallConv
  -> Safety
  -> Maybe Header
  -> DsM ([Binding], SDoc, SDoc)
dsCImport id co (CLabel cid) cconv _ _ = do
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
dsCImport id co (CFunction target) cconv@PrimCallConv safety _ =
  dsPrimCall id co (CCall (CCallSpec target cconv safety))
dsCImport id co (CFunction target) cconv safety mHeader =
  dsFCall id co (CCall (CCallSpec target cconv safety)) mHeader
dsCImport id co CWrapper cconv _ _ = dsFExportDynamic id co cconv

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

dsFExport
  :: Id
  -> Coercion
  -> CLabelString
  -> CCallConv
  -> Bool
  -> DsM (SDoc, SDoc, String, Int)
dsFExport fn_id co ext_name cconv isDyn = do
  let ty = pSnd $ coercionKind co
      (bndrs, orig_res_ty) = tcSplitPiTys ty
      fe_arg_tys' = mapMaybe binderRelevantType_maybe bndrs
      fe_arg_tys
        | isDyn = tail fe_arg_tys'
        | otherwise = fe_arg_tys'
      (res_ty, is_IO_res_ty) =
        case tcSplitIOType_maybe orig_res_ty of
          Just (_ioTyCon, res_ty) -> (res_ty, True)
          Nothing -> (orig_res_ty, False)
  dflags <- getDynFlags
  return
    $ mkFExportCBits
        dflags
        ext_name
        ( if isDyn
          then Nothing
          else Just fn_id
          )
        fe_arg_tys
        res_ty
        is_IO_res_ty
        cconv

dsFExportDynamic :: Id -> Coercion -> CCallConv -> DsM ([Binding], SDoc, SDoc)
dsFExportDynamic id co0 cconv = do
  mod <- getModule
  dflags <- getDynFlags
  let fe_nm =
        mkFastString
          $ zEncodeString (moduleStableString mod ++ "$" ++ toCName dflags id)
  cback <- newSysLocalDs arg_ty
  newStablePtrId <- dsLookupGlobalId newStablePtrName
  stable_ptr_tycon <- dsLookupTyCon stablePtrTyConName
  let stable_ptr_ty = mkTyConApp stable_ptr_tycon [arg_ty]
      export_ty = mkFunTy stable_ptr_ty arg_ty
  bindIOId <- dsLookupGlobalId bindIOName
  stbl_value <- newSysLocalDs stable_ptr_ty
  (h_code, c_code, typestring, args_size) <-
    dsFExport id (mkRepReflCo export_ty) fe_nm cconv True
  let adj_args =
        [ mkIntLitInt dflags (ccallConvToInt cconv),
          Var stbl_value,
          Lit (MachLabel fe_nm mb_sz_args IsFunction),
          Lit (mkMachString typestring)
          ]
      adjustor = fsLit "createAdjustor"
      mb_sz_args =
        case cconv of
          StdCallConv -> Just args_size
          _ -> Nothing
  ccall_adj <- dsCCall adjustor adj_args PlayRisky (mkTyConApp io_tc [res_ty])
  let io_app =
        mkLams tvs
          $ Lam cback
          $ mkApps
              (Var bindIOId)
              [ Type stable_ptr_ty,
                Type res_ty,
                mkApps (Var newStablePtrId) [Type arg_ty, Var cback],
                Lam stbl_value ccall_adj
                ]
      fed = (id `setInlineActivation` NeverActive, Cast io_app co0)
  return ([fed], h_code, c_code)
  where
    ty = pFst (coercionKind co0)
    (tvs, sans_foralls) = tcSplitForAllTys ty
    ([arg_ty], fn_res_ty) = tcSplitFunTys sans_foralls
    Just (io_tc, res_ty) = tcSplitIOType_maybe fn_res_ty

toCName :: DynFlags -> Id -> String
toCName dflags i = showSDoc dflags (pprCode CStyle (ppr (idName i)))

mkFExportCBits
  :: DynFlags
  -> FastString
  -> Maybe Id
  -> [Type]
  -> Type
  -> Bool
  -> CCallConv
  -> (SDoc, SDoc, String, Int)
mkFExportCBits dflags c_nm maybe_target arg_htys res_hty is_IO_res_ty cc =
  ( header_bits,
    c_bits,
    type_string,
    sum [widthInBytes (typeWidth rep) | (_, _, _, rep) <- aug_arg_info]
    )
  where
    arg_info :: [(SDoc, SDoc, Type, CmmType)]
    arg_info =
      [ let stg_type = showStgType ty
         in ( arg_cname n stg_type,
              stg_type,
              ty,
              typeCmmType dflags (getPrimTyOf ty)
              )
        | (ty, n) <- zip arg_htys [1 :: Int ..]
        ]
    arg_cname n stg_ty
      | libffi =
        char '*'
          <> parens (stg_ty <> char '*')
          <> text "args"
          <> brackets (int (n - 1))
      | otherwise = text ('a' : show n)
    libffi = cLibFFI && isNothing maybe_target
    type_string
      | libffi = primTyDescChar dflags res_hty : arg_type_string
      | otherwise = arg_type_string
    arg_type_string = [primTyDescChar dflags ty | (_, _, ty, _) <- arg_info]
    aug_arg_info
      | isNothing maybe_target =
        stable_ptr_arg : insertRetAddr dflags cc arg_info
      | otherwise = arg_info
    stable_ptr_arg =
      ( text "the_stableptr",
        text "StgStablePtr",
        undefined,
        typeCmmType dflags (mkStablePtrPrimTy alphaTy)
        )
    res_hty_is_unit = res_hty `eqType` unitTy
    cResType
      | res_hty_is_unit = text "void"
      | otherwise = showStgType res_hty
    ffi_cResType
      | is_ffi_arg_type = text "ffi_arg"
      | otherwise = cResType
      where
        res_ty_key = getUnique (getName (typeTyCon res_hty))
        is_ffi_arg_type =
          res_ty_key
            `notElem` [floatTyConKey, doubleTyConKey, int64TyConKey, word64TyConKey]
    pprCconv = ccallConvAttribute cc
    header_bits = text "extern" <+> fun_proto <> semi
    fun_args
      | null aug_arg_info = text "void"
      | otherwise =
        hsep $ punctuate comma $ map (\(nm, ty, _, _) -> ty <+> nm) aug_arg_info
    fun_proto
      | libffi =
        text "void"
          <+> ftext c_nm
          <> parens
               ( text
                   "void *cif STG_UNUSED, void* resp, void** args, void* the_stableptr"
                 )
      | otherwise = cResType <+> pprCconv <+> ftext c_nm <> parens fun_args
    the_cfun =
      case maybe_target of
        Nothing -> text "(StgClosure*)deRefStablePtr(the_stableptr)"
        Just hs_fn -> char '&' <> ppr hs_fn <> text "_closure"
    cap = text "cap" <> comma
    expr_to_run = foldl appArg the_cfun arg_info
      where
        appArg acc (arg_cname, _, arg_hty, _) =
          text "rts_apply"
            <> parens
                 (cap <> acc <> comma <> mkHObj arg_hty <> parens (cap <> arg_cname))
    declareResult = text "HaskellObj ret;"
    declareCResult
      | res_hty_is_unit = empty
      | otherwise = cResType <+> text "cret;"
    assignCResult
      | res_hty_is_unit = empty
      | otherwise =
        text "cret=" <> unpackHObj res_hty <> parens (text "ret") <> semi
    extern_decl =
      case maybe_target of
        Nothing -> empty
        Just hs_fn ->
          text "extern StgClosure " <> ppr hs_fn <> text "_closure" <> semi
    c_bits =
      space $$ extern_decl $$ fun_proto
        $$ vcat
             [ lbrace,
               text "Capability *cap;",
               declareResult,
               declareCResult,
               text "cap = rts_lock();",
               text "rts_evalIO"
                 <> parens
                      ( char '&'
                          <> cap
                          <> text "rts_apply"
                          <> parens
                               ( cap
                                   <> text "(HaskellObj)"
                                   <> ptext
                                        ( if is_IO_res_ty
                                          then sLit "runIO_closure"
                                          else sLit "runNonIO_closure"
                                          )
                                   <> comma
                                   <> expr_to_run
                                 )
                          <+> comma
                          <> text "&ret"
                        )
                 <> semi,
               text "rts_checkSchedStatus"
                 <> parens (doubleQuotes (ftext c_nm) <> comma <> text "cap")
                 <> semi,
               assignCResult,
               text "rts_unlock(cap);",
               ppUnless res_hty_is_unit
                 $ if libffi
                 then
                   char '*'
                     <> parens (ffi_cResType <> char '*')
                     <> text "resp = cret;"
                 else text "return cret;",
               rbrace
               ]

foreignExportInitialiser :: Id -> SDoc
foreignExportInitialiser hs_fn =
  vcat
    [ text "static void stginit_export_"
        <> ppr hs_fn
        <> text "() __attribute__((constructor));",
      text "static void stginit_export_" <> ppr hs_fn <> text "()",
      braces
        ( text "foreignExportStablePtr"
            <> parens (text "(StgPtr) &" <> ppr hs_fn <> text "_closure")
            <> semi
          )
      ]

mkHObj :: Type -> SDoc
mkHObj t = text "rts_mk" <> text (showFFIType t)

unpackHObj :: Type -> SDoc
unpackHObj t = text "rts_get" <> text (showFFIType t)

showStgType :: Type -> SDoc
showStgType t = text "Hs" <> text (showFFIType t)

showFFIType :: Type -> String
showFFIType t = getOccString (getName (typeTyCon t))

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

typeTyCon :: Type -> TyCon
typeTyCon ty
  | Just (tc, _) <- tcSplitTyConApp_maybe (unwrapType ty) = tc
  | otherwise = pprPanic "DsForeign.typeTyCon" (ppr ty)

insertRetAddr
  :: DynFlags
  -> CCallConv
  -> [(SDoc, SDoc, Type, CmmType)]
  -> [(SDoc, SDoc, Type, CmmType)]
insertRetAddr dflags CCallConv args =
  case platformArch platform of
    ArchX86_64
      | platformOS platform == OSMinGW32 ->
        let go
              :: Int
              -> [(SDoc, SDoc, Type, CmmType)]
              -> [(SDoc, SDoc, Type, CmmType)]
            go 4 args = retAddrArg dflags : args
            go n (arg : args) = arg : go (n + 1) args
            go _ [] = []
         in go 0 args
      | otherwise ->
        let go
              :: Int
              -> [(SDoc, SDoc, Type, CmmType)]
              -> [(SDoc, SDoc, Type, CmmType)]
            go 6 args = retAddrArg dflags : args
            go n (arg@(_, _, _, rep) : args)
              | cmmEqType_ignoring_ptrhood rep b64 = arg : go (n + 1) args
              | otherwise = arg : go n args
            go _ [] = []
         in go 0 args
    _ -> retAddrArg dflags : args
  where
    platform = targetPlatform dflags
insertRetAddr _ _ args = args

retAddrArg :: DynFlags -> (SDoc, SDoc, Type, CmmType)
retAddrArg dflags =
  ( text "original_return_addr",
    text "void*",
    undefined,
    typeCmmType dflags addrPrimTy
    )

getPrimTyOf :: Type -> UnaryType
getPrimTyOf ty
  | isBoolTy rep_ty = intPrimTy
  | otherwise =
    case splitDataProductType_maybe rep_ty of
      Just (_, _, _, [prim_ty]) -> prim_ty
      _other -> pprPanic "DsForeign.getPrimTyOf" (ppr ty)
  where
    rep_ty = unwrapType ty

primTyDescChar :: DynFlags -> Type -> Char
primTyDescChar dflags ty
  | ty `eqType` unitTy = 'v'
  | otherwise =
    case typePrimRep1 (getPrimTyOf ty) of
      IntRep -> signed_word
      WordRep -> unsigned_word
      Int64Rep -> 'L'
      Word64Rep -> 'l'
      AddrRep -> 'p'
      FloatRep -> 'f'
      DoubleRep -> 'd'
      _ -> pprPanic "primTyDescChar" (ppr ty)
  where
    (signed_word, unsigned_word)
      | wORD_SIZE dflags == 4 = ('W', 'w')
      | wORD_SIZE dflags == 8 = ('L', 'l')
      | otherwise = panic "primTyDescChar"
