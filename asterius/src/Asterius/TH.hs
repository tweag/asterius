{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Asterius.TH
  ( asteriusRunMeta
    )
where

import Annotations
import Bag
import BasicTypes hiding (SuccessFlag (..))
import Class
import CoAxiom
import ConLike
import Control.Exception
import Convert
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Data (Data)
import Data.Dynamic
  ( fromDynamic,
    toDyn
    )
import Data.List (find)
import qualified Data.Map as Map
import Data.Maybe
import Data.Proxy (Proxy (..))
import Data.Typeable
  ( TypeRep,
    Typeable,
    typeOf,
    typeRep
    )
import DataCon
import DsExpr
import DsMonad
import DynFlags
import qualified EnumSet
import ErrUtils
import FV
import FamInst
import FamInstEnv
import FastString
import FileCleanup
  ( TempFileLifetime (..),
    newTempName
    )
import Finder
import GHC.Serialized
import GHCi
import GHCi.Message
import GHCi.RemoteTypes
import HsSyn
import HscMain
import HscTypes
import Id
import IdInfo
import InstEnv
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import Lexeme
import LoadIface
import Maybes (MaybeErr (..))
import Module
import Name
import NameEnv
import NameSet
import OccName
import Outputable
import Panic
import PatSyn
import Plugins
import PrelNames
import RdrName
import RnEnv
import RnFixity (lookupFixityRn_help)
import RnTypes
import RnUtils (HsDocContext (..))
import SrcLoc
import TcEnv
import TcHsSyn
import TcHsType
import TcMType
import TcRnMonad
import TcType
import TyCoRep
import TyCon
import Type
import TysWiredIn
import Unique
import Util
import Var
import VarSet
import Prelude hiding ((<>))

asteriusConvertAnnotationWrapper
  :: ForeignHValue -> TcM (Either MsgDoc Serialized)
asteriusConvertAnnotationWrapper fhv = Right <$> asteriusRunTH THAnnWrapper fhv

runQResult
  :: (a -> String)
  -> (SrcSpan -> a -> b)
  -> (ForeignHValue -> TcM a)
  -> SrcSpan
  -> ForeignHValue
  -> TcM b
runQResult show_th f runQ expr_span hval = do
  th_result <- runQ hval
  traceTc "Got TH result:" (text (show_th th_result))
  return (f expr_span th_result)

asteriusRunMeta :: MetaHook TcM
asteriusRunMeta (MetaE r) =
  fmap r
    . runMeta' True ppr (runQResult TH.pprint convertToHsExpr asteriusRunTHExp)
asteriusRunMeta (MetaP r) =
  fmap r
    . runMeta' True ppr (runQResult TH.pprint convertToPat asteriusRunTHPat)
asteriusRunMeta (MetaT r) =
  fmap r
    . runMeta' True ppr (runQResult TH.pprint convertToHsType asteriusRunTHType)
asteriusRunMeta (MetaD r) =
  fmap r
    . runMeta' True ppr (runQResult TH.pprint convertToHsDecls asteriusRunTHDec)
asteriusRunMeta (MetaAW r) =
  fmap r . runMeta' False (const empty) (const asteriusConvertAnnotationWrapper)

runMeta'
  :: Bool
  -> (hs_syn -> SDoc)
  -> (SrcSpan -> ForeignHValue -> TcM (Either MsgDoc hs_syn))
  -> LHsExpr GhcTc
  -> TcM hs_syn
runMeta' show_code ppr_hs run_and_convert expr = do
  traceTc "About to run" (ppr expr)
  recordThSpliceUse
  failIfErrsM
  hsc_env <- getTopEnv
  expr' <- withPlugins (hsc_dflags hsc_env) spliceRunAction expr
  ds_expr <- initDsTc (dsLExpr expr')
  src_span <- getSrcSpanM
  traceTc "About to run (desugared)" (ppr ds_expr)
  either_hval <-
    tryM $ liftIO
      $ HscMain.hscCompileCoreExpr hsc_env
          src_span
          ds_expr
  case either_hval of
    Left exn -> fail_with_exn "compile and link" exn
    Right hval -> do
      let expr_span = getLoc expr
      either_tval <-
        tryAllM $ setSrcSpan expr_span $ do
          mb_result <- run_and_convert expr_span hval
          case mb_result of
            Left err -> failWithTc err
            Right result -> do
              traceTc "Got HsSyn result:" (ppr_hs result)
              return $! result
      case either_tval of
        Right v -> return v
        Left se -> case fromException se of
          Just IOEnvFailure -> failM
          _ -> fail_with_exn "run" se
  where
    fail_with_exn :: Exception e => String -> e -> TcM a
    fail_with_exn phase exn = do
      exn_msg <- liftIO $ Panic.safeShowException exn
      let msg =
            vcat
              [ text "Exception when trying to" <+> text phase
                  <+> text
                        "compile-time code:",
                nest 2 (text exn_msg),
                if show_code then text "Code:" <+> ppr expr else empty
                ]
      failWithTc msg

instance TH.Quasi TcM where

  qNewName s = do
    u <- newUnique
    let i = getKey u
    return (TH.mkNameU s i)

  qReport True msg = seqList msg $ addErr (text msg)
  qReport False msg = seqList msg $ addWarn NoReason (text msg)

  qLocation = do
    m <- getModule
    l <- getSrcSpanM
    r <-
      case l of
        UnhelpfulSpan _ -> pprPanic "qLocation: Unhelpful location" (ppr l)
        RealSrcSpan s -> return s
    return
      ( TH.Loc
          { TH.loc_filename = unpackFS (srcSpanFile r),
            TH.loc_module = moduleNameString (moduleName m),
            TH.loc_package = unitIdString (moduleUnitId m),
            TH.loc_start = (srcSpanStartLine r, srcSpanStartCol r),
            TH.loc_end = (srcSpanEndLine r, srcSpanEndCol r)
            }
        )

  qLookupName = lookupName

  qReify = reify

  qReifyFixity nm = lookupThName nm >>= reifyFixity

  qReifyInstances = reifyInstances

  qReifyRoles = reifyRoles

  qReifyAnnotations = reifyAnnotations

  qReifyModule = reifyModule

  qReifyConStrictness nm = do
    nm' <- lookupThName nm
    dc <- tcLookupDataCon nm'
    let bangs = dataConImplBangs dc
    return (map reifyDecidedStrictness bangs)

  qRecover = tryTcDiscardingErrs

  qAddDependentFile fp = do
    ref <- fmap tcg_dependent_files getGblEnv
    dep_files <- readTcRef ref
    writeTcRef ref (fp : dep_files)

  qAddTempFile suffix = do
    dflags <- getDynFlags
    liftIO $ newTempName dflags TFL_GhcSession suffix

  qAddTopDecls thds = do
    l <- getSrcSpanM
    let either_hval = convertToHsDecls l thds
    ds <-
      case either_hval of
        Left exn ->
          pprPanic "qAddTopDecls: can't convert top-level declarations" exn
        Right ds -> return ds
    mapM_ (checkTopDecl . unLoc) ds
    th_topdecls_var <- fmap tcg_th_topdecls getGblEnv
    updTcRef th_topdecls_var (ds ++)
    where
      checkTopDecl :: HsDecl GhcPs -> TcM ()
      checkTopDecl (ValD _ binds) = mapM_ bindName (collectHsBindBinders binds)
      checkTopDecl (SigD _ _) = return ()
      checkTopDecl (AnnD _ _) = return ()
      checkTopDecl (ForD _ ForeignImport {fd_name = L _ name}) = bindName name
      checkTopDecl _ =
        addErr
          $ text
              "Only function, value, annotation, and foreign import declarations may be added with addTopDecl"
      bindName :: RdrName -> TcM ()
      bindName (Exact n) = do
        th_topnames_var <- fmap tcg_th_topnames getGblEnv
        updTcRef th_topnames_var (`extendNameSet` n)
      bindName name =
        addErr
          $ hang
              ( text "The binder" <+> quotes (ppr name)
                  <+> ptext
                        (sLit "is not a NameU.")
                )
              2
              ( text
                  "Probable cause: you used mkName instead of newName to generate a binding."
                )

  qAddForeignFilePath lang fp = do
    var <- fmap tcg_th_foreign_files getGblEnv
    updTcRef var ((lang, fp) :)

  qAddModFinalizer fin = do
    r <- liftIO $ mkRemoteRef fin
    fref <- liftIO $ mkForeignRef r (freeRemoteRef r)
    addModFinalizerRef fref

  qAddCorePlugin plugin = do
    hsc_env <- env_top <$> getEnv
    r <- liftIO $ findHomeModule hsc_env (mkModuleName plugin)
    let err =
          hang
            (text "addCorePlugin: invalid plugin module " <+> text (show plugin))
            2
            (text "Plugins in the current package can't be specified.")
    case r of
      Found {} -> addErr err
      FoundMultiple {} -> addErr err
      _ -> return ()
    th_coreplugins_var <- tcg_th_coreplugins <$> getGblEnv
    updTcRef th_coreplugins_var (plugin :)

  qGetQ :: forall a. Typeable a => TcM (Maybe a)
  qGetQ = do
    th_state_var <- fmap tcg_th_state getGblEnv
    th_state <- readTcRef th_state_var
    return (Map.lookup (typeRep (Proxy :: Proxy a)) th_state >>= fromDynamic)

  qPutQ x = do
    th_state_var <- fmap tcg_th_state getGblEnv
    updTcRef th_state_var (Map.insert (typeOf x) (toDyn x))

  qIsExtEnabled = xoptM

  qExtsEnabled = EnumSet.toList . extensionFlags . hsc_dflags <$> getTopEnv

addModFinalizerRef :: ForeignRef (TH.Q ()) -> TcM ()
addModFinalizerRef finRef = do
  th_stage <- getStage
  case th_stage of
    RunSplice th_modfinalizers_var -> updTcRef th_modfinalizers_var (finRef :)
    _ ->
      pprPanic
        "addModFinalizer was called when no finalizers were collected"
        (ppr th_stage)

asteriusRunTHExp :: ForeignHValue -> TcM TH.Exp
asteriusRunTHExp = asteriusRunTH THExp

asteriusRunTHPat :: ForeignHValue -> TcM TH.Pat
asteriusRunTHPat = asteriusRunTH THPat

asteriusRunTHType :: ForeignHValue -> TcM TH.Type
asteriusRunTHType = asteriusRunTH THType

asteriusRunTHDec :: ForeignHValue -> TcM [TH.Dec]
asteriusRunTHDec = asteriusRunTH THDec

asteriusRunTH :: Binary a => THResultType -> ForeignHValue -> TcM a
asteriusRunTH ty fhv = do
  hsc_env <- env_top <$> getEnv
  withIServ hsc_env $ \i -> do
    rstate <- getTHState i
    loc <- TH.qLocation
    liftIO $ withForeignRef rstate $ \state_hv -> withForeignRef fhv
      $ \q_hv -> writeIServ i (putMessage (RunTH state_hv q_hv ty (Just loc)))
    runRemoteTH i []
    bs <- readQResult i
    return $! runGet get (LB.fromStrict bs)

runRemoteTH :: IServ -> [Messages] -> TcM ()
runRemoteTH iserv recovers = do
  THMsg msg <- liftIO $ readIServ iserv getTHMessage
  case msg of
    RunTHDone -> return ()
    StartRecover -> do
      v <- getErrsVar
      msgs <- readTcRef v
      writeTcRef v emptyMessages
      runRemoteTH iserv (msgs : recovers)
    EndRecover caught_error -> do
      let (prev_msgs@(prev_warns, prev_errs), rest) = case recovers of
            [] -> panic "EndRecover"
            a : b -> (a, b)
      v <- getErrsVar
      (warn_msgs, _) <- readTcRef v
      writeTcRef v
        $ if caught_error
        then prev_msgs
        else (prev_warns `unionBags` warn_msgs, prev_errs)
      runRemoteTH iserv rest
    _other -> do
      r <- handleTHMessage msg
      liftIO $ writeIServ iserv (put r)
      runRemoteTH iserv recovers

readQResult :: Binary a => IServ -> TcM a
readQResult i = do
  qr <- liftIO $ readIServ i get
  case qr of
    QDone a -> return a
    QException str -> liftIO $ throwIO (ErrorCall str)
    QFail str -> fail str

getTHState :: IServ -> TcM (ForeignRef (IORef QState))
getTHState i = do
  tcg <- getGblEnv
  th_state <- readTcRef (tcg_th_remote_state tcg)
  case th_state of
    Just rhv -> return rhv
    Nothing -> do
      hsc_env <- env_top <$> getEnv
      fhv <- liftIO $ mkFinalizedHValue hsc_env =<< iservCall (hsc_dflags hsc_env) i StartTH
      writeTcRef (tcg_th_remote_state tcg) (Just fhv)
      return fhv

wrapTHResult :: TcM a -> TcM (THResult a)
wrapTHResult tcm = do
  e <- tryM tcm
  case e of
    Left e -> return (THException (show e))
    Right a -> return (THComplete a)

handleTHMessage :: THMessage a -> TcM a
handleTHMessage msg = case msg of
  NewName a -> wrapTHResult $ TH.qNewName a
  Report b str -> wrapTHResult $ TH.qReport b str
  LookupName b str -> wrapTHResult $ TH.qLookupName b str
  Reify n -> wrapTHResult $ TH.qReify n
  ReifyFixity n -> wrapTHResult $ TH.qReifyFixity n
  ReifyInstances n ts -> wrapTHResult $ TH.qReifyInstances n ts
  ReifyRoles n -> wrapTHResult $ TH.qReifyRoles n
  ReifyAnnotations lookup tyrep ->
    wrapTHResult $ map B.pack <$> getAnnotationsByTypeRep lookup tyrep
  ReifyModule m -> wrapTHResult $ TH.qReifyModule m
  ReifyConStrictness nm -> wrapTHResult $ TH.qReifyConStrictness nm
  AddDependentFile f -> wrapTHResult $ TH.qAddDependentFile f
  AddTempFile s -> wrapTHResult $ TH.qAddTempFile s
  AddModFinalizer r -> do
    hsc_env <- env_top <$> getEnv
    wrapTHResult $ liftIO (mkFinalizedHValue hsc_env r) >>= addModFinalizerRef
  AddCorePlugin str -> wrapTHResult $ TH.qAddCorePlugin str
  AddTopDecls decs -> wrapTHResult $ TH.qAddTopDecls decs
  AddForeignFilePath lang str -> wrapTHResult $ TH.qAddForeignFilePath lang str
  IsExtEnabled ext -> wrapTHResult $ TH.qIsExtEnabled ext
  ExtsEnabled -> wrapTHResult TH.qExtsEnabled
  FailIfErrs -> wrapTHResult failIfErrsM
  _ -> panic ("handleTHMessage: unexpected message " ++ show msg)

getAnnotationsByTypeRep :: TH.AnnLookup -> TypeRep -> TcM [[Word8]]
getAnnotationsByTypeRep th_name tyrep = do
  name <- lookupThAnnLookup th_name
  topEnv <- getTopEnv
  epsHptAnns <- liftIO $ prepareAnnotations topEnv Nothing
  tcg <- getGblEnv
  let selectedEpsHptAnns = findAnnsByTypeRep epsHptAnns name tyrep
  let selectedTcgAnns = findAnnsByTypeRep (tcg_ann_env tcg) name tyrep
  return (selectedEpsHptAnns ++ selectedTcgAnns)

reifyInstances :: TH.Name -> [TH.Type] -> TcM [TH.Dec]
reifyInstances th_nm th_tys =
  addErrCtxt
    ( text "In the argument of reifyInstances:" <+> pprTh th_nm
        <+> sep
              (map pprTh th_tys)
      )
    $ do
      loc <- getSrcSpanM
      rdr_ty <- cvt loc (mkThAppTs (TH.ConT th_nm) th_tys)
      free_vars <- extractHsTyRdrTyVars rdr_ty
      let tv_rdrs = freeKiTyVarsAllVars free_vars
      ((tv_names, rn_ty), _fvs) <-
        checkNoErrs $ bindLRdrNames tv_rdrs $ \tv_names -> do
          (rn_ty, fvs) <- rnLHsType doc rdr_ty
          return ((tv_names, rn_ty), fvs)
      (_tvs, ty) <-
        solveEqualities
          $ tcImplicitTKBndrs ReifySkol tv_names
          $ fst
          <$> tcLHsType rn_ty
      ty <- zonkTcTypeToType emptyZonkEnv ty
      traceTc "reifyInstances" (ppr ty $$ ppr (typeKind ty))
      case splitTyConApp_maybe ty of
        Just (tc, tys)
          | Just cls <- tyConClass_maybe tc ->
            do
              inst_envs <- tcGetInstEnvs
              let (matches, unifies, _) = lookupInstEnv False inst_envs cls tys
              traceTc "reifyInstances1" (ppr matches)
              reifyClassInstances cls (map fst matches ++ unifies)
          | isOpenFamilyTyCon tc ->
            do
              inst_envs <- tcGetFamInstEnvs
              let matches = lookupFamInstEnv inst_envs tc tys
              traceTc "reifyInstances2" (ppr matches)
              reifyFamilyInstances tc (map fim_instance matches)
        _ ->
          bale_out
            ( hang
                (text "reifyInstances:" <+> quotes (ppr ty))
                2
                (text "is not a class constraint or type family application")
              )
  where
    doc = ClassInstanceCtx
    bale_out = failWithTc
    cvt :: SrcSpan -> TH.Type -> TcM (LHsType GhcPs)
    cvt loc th_ty = case convertToHsType loc th_ty of
      Left msg -> failWithTc msg
      Right ty -> return ty

lookupName :: Bool -> String -> TcM (Maybe TH.Name)
lookupName is_type_name s = do
  lcl_env <- getLocalRdrEnv
  case lookupLocalRdrEnv lcl_env rdr_name of
    Just n -> return (Just (reifyName n))
    Nothing -> do
      mb_nm <- lookupGlobalOccRn_maybe rdr_name
      return (fmap reifyName mb_nm)
  where
    th_name = TH.mkName s
    occ_fs :: FastString
    occ_fs = mkFastString (TH.nameBase th_name)
    occ :: OccName
    occ
      | is_type_name =
        if isLexVarSym occ_fs || isLexCon occ_fs
        then mkTcOccFS occ_fs
        else mkTyVarOccFS occ_fs
      | otherwise =
        if isLexCon occ_fs
        then mkDataOccFS occ_fs
        else mkVarOccFS occ_fs
    rdr_name = case TH.nameModule th_name of
      Nothing -> mkRdrUnqual occ
      Just mod -> mkRdrQual (mkModuleName mod) occ

getThing :: TH.Name -> TcM TcTyThing
getThing th_name = do
  name <- lookupThName th_name
  traceIf
    ( text "reify"
        <+> text (show th_name)
        <+> brackets (ppr_ns th_name)
        <+> ppr name
      )
  tcLookupTh name
  where
    ppr_ns (TH.Name _ (TH.NameG TH.DataName _pkg _mod)) = text "data"
    ppr_ns (TH.Name _ (TH.NameG TH.TcClsName _pkg _mod)) = text "tc"
    ppr_ns (TH.Name _ (TH.NameG TH.VarName _pkg _mod)) = text "var"
    ppr_ns _ = panic "reify/ppr_ns"

reify :: TH.Name -> TcM TH.Info
reify th_name = do
  traceTc "reify 1" (text (TH.showName th_name))
  thing <- getThing th_name
  traceTc "reify 2" (ppr thing)
  reifyThing thing

lookupThName :: TH.Name -> TcM Name
lookupThName th_name = do
  mb_name <- lookupThNameMaybe th_name
  case mb_name of
    Nothing -> failWithTc (notInScope th_name)
    Just name -> return name

lookupThNameMaybe :: TH.Name -> TcM (Maybe Name)
lookupThNameMaybe th_name = do
  names <- mapMaybeM lookup (thRdrNameGuesses th_name)
  return (listToMaybe names)
  where
    lookup rdr_name = do
      rdr_env <- getLocalRdrEnv
      case lookupLocalRdrEnv rdr_env rdr_name of
        Just name -> return (Just name)
        Nothing -> lookupGlobalOccRn_maybe rdr_name

tcLookupTh :: Name -> TcM TcTyThing
tcLookupTh name = do
  (gbl_env, lcl_env) <- getEnvs
  case lookupNameEnv (tcl_env lcl_env) name of
    Just thing -> return thing
    Nothing -> case lookupNameEnv (tcg_type_env gbl_env) name of
      Just thing -> return (AGlobal thing)
      Nothing ->
        if nameIsLocalOrFrom (tcg_semantic_mod gbl_env) name
        then failWithTc (notInEnv name)
        else
          do
            mb_thing <- tcLookupImported_maybe name
            case mb_thing of
              Succeeded thing -> return (AGlobal thing)
              Failed msg -> failWithTc msg

notInScope :: TH.Name -> SDoc
notInScope th_name =
  quotes (text (TH.pprint th_name)) <+> text "is not in scope at a reify"

notInEnv :: Name -> SDoc
notInEnv name =
  quotes (ppr name) <+> text "is not in the type environment at a reify"

reifyRoles :: TH.Name -> TcM [TH.Role]
reifyRoles th_name = do
  thing <- getThing th_name
  case thing of
    AGlobal (ATyCon tc) -> return (map reify_role (tyConRoles tc))
    _ -> failWithTc (text "No roles associated with" <+> ppr thing)
  where
    reify_role Nominal = TH.NominalR
    reify_role Representational = TH.RepresentationalR
    reify_role Phantom = TH.PhantomR

reifyThing :: TcTyThing -> TcM TH.Info
reifyThing (AGlobal (AnId id)) = do
  ty <- reifyType (idType id)
  let v = reifyName id
  case idDetails id of
    ClassOpId cls -> return (TH.ClassOpI v ty (reifyName cls))
    RecSelId {sel_tycon = RecSelData tc} ->
      return (TH.VarI (reifySelector id tc) ty Nothing)
    _ -> return (TH.VarI v ty Nothing)
reifyThing (AGlobal (ATyCon tc)) = reifyTyCon tc
reifyThing (AGlobal (AConLike (RealDataCon dc))) = do
  let name = dataConName dc
  ty <- reifyType (idType (dataConWrapId dc))
  return (TH.DataConI (reifyName name) ty (reifyName (dataConOrigTyCon dc)))
reifyThing (AGlobal (AConLike (PatSynCon ps))) = do
  let name = reifyName ps
  ty <- reifyPatSynType (patSynSig ps)
  return (TH.PatSynI name ty)
reifyThing ATcId {tct_id = id} = do
  ty1 <- zonkTcType (idType id)
  ty2 <- reifyType ty1
  return (TH.VarI (reifyName id) ty2 Nothing)
reifyThing (ATyVar tv tv1) = do
  ty1 <- zonkTcTyVar tv1
  ty2 <- reifyType ty1
  return (TH.TyVarI (reifyName tv) ty2)
reifyThing thing = pprPanic "reifyThing" (pprTcTyThingCategory thing)

reifyAxBranch :: TyCon -> CoAxBranch -> TcM TH.TySynEqn
reifyAxBranch fam_tc CoAxBranch {cab_lhs = lhs, cab_rhs = rhs} = do
  let lhs_types_only = filterOutInvisibleTypes fam_tc lhs
  lhs' <- reifyTypes lhs_types_only
  annot_th_lhs <-
    zipWith3M annotThType
      (mkIsPolyTvs fam_tvs)
      lhs_types_only
      lhs'
  rhs' <- reifyType rhs
  return (TH.TySynEqn annot_th_lhs rhs')
  where
    fam_tvs = tyConVisibleTyVars fam_tc

reifyTyCon :: TyCon -> TcM TH.Info
reifyTyCon tc
  | Just cls <- tyConClass_maybe tc = reifyClass cls
  | isFunTyCon tc = return (TH.PrimTyConI (reifyName tc) 2 False)
  | isPrimTyCon tc =
    return
      (TH.PrimTyConI (reifyName tc) (tyConArity tc) (isUnliftedTyCon tc))
  | isTypeFamilyTyCon tc =
    do
      let tvs = tyConTyVars tc
          res_kind = tyConResKind tc
          resVar = famTcResVar tc
      kind' <- reifyKind res_kind
      let (resultSig, injectivity) = case resVar of
            Nothing -> (TH.KindSig kind', Nothing)
            Just name ->
              let thName = reifyName name
                  injAnnot = tyConInjectivityInfo tc
                  sig = TH.TyVarSig (TH.KindedTV thName kind')
                  inj = case injAnnot of
                    NotInjective -> Nothing
                    Injective ms -> Just (TH.InjectivityAnn thName injRHS)
                      where
                        injRHS = map (reifyName . tyVarName) (filterByList ms tvs)
               in (sig, inj)
      tvs' <- reifyTyVars (tyConVisibleTyVars tc)
      let tfHead = TH.TypeFamilyHead (reifyName tc) tvs' resultSig injectivity
      if isOpenTypeFamilyTyCon tc
      then
        do
          fam_envs <- tcGetFamInstEnvs
          instances <- reifyFamilyInstances tc (familyInstances fam_envs tc)
          return (TH.FamilyI (TH.OpenTypeFamilyD tfHead) instances)
      else
        do
          eqns <-
            case isClosedSynFamilyTyConWithAxiom_maybe tc of
              Just ax ->
                mapM (reifyAxBranch tc) $ fromBranches $ coAxiomBranches ax
              Nothing -> return []
          return (TH.FamilyI (TH.ClosedTypeFamilyD tfHead eqns) [])
  | isDataFamilyTyCon tc =
    do
      let res_kind = tyConResKind tc
      kind' <- fmap Just (reifyKind res_kind)
      tvs' <- reifyTyVars (tyConVisibleTyVars tc)
      fam_envs <- tcGetFamInstEnvs
      instances <- reifyFamilyInstances tc (familyInstances fam_envs tc)
      return (TH.FamilyI (TH.DataFamilyD (reifyName tc) tvs' kind') instances)
  | Just (_, rhs) <- synTyConDefn_maybe tc =
    do
      rhs' <- reifyType rhs
      tvs' <- reifyTyVars (tyConVisibleTyVars tc)
      return (TH.TyConI (TH.TySynD (reifyName tc) tvs' rhs'))
  | otherwise =
    do
      cxt <- reifyCxt (tyConStupidTheta tc)
      let tvs = tyConTyVars tc
          dataCons = tyConDataCons tc
          isGadt = isGadtSyntaxTyCon tc
      cons <- mapM (reifyDataCon isGadt (mkTyVarTys tvs)) dataCons
      r_tvs <- reifyTyVars (tyConVisibleTyVars tc)
      let name = reifyName tc
          deriv = []
          decl
            | isNewTyCon tc = TH.NewtypeD cxt name r_tvs Nothing (head cons) deriv
            | otherwise = TH.DataD cxt name r_tvs Nothing cons deriv
      return (TH.TyConI decl)

reifyDataCon :: Bool -> [Type] -> DataCon -> TcM TH.Con
reifyDataCon isGadtDataCon tys dc = do
  let (ex_tvs, theta, arg_tys) = dataConInstSig dc tys
      g_user_tvs' = dataConUserTyVars dc
      (g_univ_tvs, _, g_eq_spec, g_theta', g_arg_tys', g_res_ty') =
        dataConFullSig dc
      (srcUnpks, srcStricts) = mapAndUnzip reifySourceBang (dataConSrcBangs dc)
      dcdBangs = zipWith TH.Bang srcUnpks srcStricts
      fields = dataConFieldLabels dc
      name = reifyName dc
      eq_spec_tvs = mkVarSet (map eqSpecTyVar g_eq_spec)
  (univ_subst, _) <-
    freshenTyVarBndrs
      $ filterOut (`elemVarSet` eq_spec_tvs) g_univ_tvs
  let (tvb_subst, g_user_tvs) = substTyVarBndrs univ_subst g_user_tvs'
      g_theta = substTys tvb_subst g_theta'
      g_arg_tys = substTys tvb_subst g_arg_tys'
      g_res_ty = substTy tvb_subst g_res_ty'
  r_arg_tys <- reifyTypes (if isGadtDataCon then g_arg_tys else arg_tys)
  main_con <-
    if | not (null fields) && not isGadtDataCon ->
         return
           $ TH.RecC name (zip3 (map reifyFieldLabel fields) dcdBangs r_arg_tys)
       | not (null fields) ->
         do
           res_ty <- reifyType g_res_ty
           return
             $ TH.RecGadtC
                 [name]
                 (zip3 (map (reifyName . flSelector) fields) dcdBangs r_arg_tys)
                 res_ty
       | dataConIsInfix dc && not isGadtDataCon ->
         do
           let [r_a1, r_a2] = r_arg_tys
               [s1, s2] = dcdBangs
           return $ TH.InfixC (s1, r_a1) name (s2, r_a2)
       | isGadtDataCon ->
         do
           res_ty <- reifyType g_res_ty
           return $ TH.GadtC [name] (dcdBangs `zip` r_arg_tys) res_ty
       | otherwise -> return $ TH.NormalC name (dcdBangs `zip` r_arg_tys)
  let (ex_tvs', theta')
        | isGadtDataCon = (g_user_tvs, g_theta)
        | otherwise = (ex_tvs, theta)
      ret_con
        | null ex_tvs' && null theta' = return main_con
        | otherwise =
          do
            cxt <- reifyCxt theta'
            ex_tvs'' <- reifyTyVars ex_tvs'
            return (TH.ForallC ex_tvs'' cxt main_con)
  ret_con

reifyClass :: Class -> TcM TH.Info
reifyClass cls = do
  cxt <- reifyCxt theta
  inst_envs <- tcGetInstEnvs
  insts <- reifyClassInstances cls (InstEnv.classInstances inst_envs cls)
  assocTys <- concatMapM reifyAT ats
  ops <- concatMapM reify_op op_stuff
  tvs' <- reifyTyVars (tyConVisibleTyVars (classTyCon cls))
  let dec = TH.ClassD cxt (reifyName cls) tvs' fds' (assocTys ++ ops)
  return (TH.ClassI dec insts)
  where
    (_, fds, theta, _, ats, op_stuff) = classExtraBigSig cls
    fds' = map reifyFunDep fds
    reify_op (op, def_meth) = do
      ty <- reifyType (idType op)
      let nm' = reifyName op
      case def_meth of
        Just (_, GenericDM gdm_ty) -> do
          gdm_ty' <- reifyType gdm_ty
          return [TH.SigD nm' ty, TH.DefaultSigD nm' gdm_ty']
        _ -> return [TH.SigD nm' ty]
    reifyAT :: ClassATItem -> TcM [TH.Dec]
    reifyAT (ATI tycon def) = do
      tycon' <- reifyTyCon tycon
      case tycon' of
        TH.FamilyI dec _ -> do
          let (tyName, tyArgs) = tfNames dec
          (dec :)
            <$> maybe (return [])
                  (fmap (: []) . reifyDefImpl tyName tyArgs . fst)
                  def
        _ -> pprPanic "reifyAT" (text (show tycon'))
    reifyDefImpl :: TH.Name -> [TH.Name] -> Type -> TcM TH.Dec
    reifyDefImpl n args ty =
      TH.TySynInstD n . TH.TySynEqn (map TH.VarT args) <$> reifyType ty
    tfNames :: TH.Dec -> (TH.Name, [TH.Name])
    tfNames (TH.OpenTypeFamilyD (TH.TypeFamilyHead n args _ _)) =
      (n, map bndrName args)
    tfNames d = pprPanic "tfNames" (text (show d))
    bndrName :: TH.TyVarBndr -> TH.Name
    bndrName (TH.PlainTV n) = n
    bndrName (TH.KindedTV n _) = n

annotThType :: Bool -> TyCoRep.Type -> TH.Type -> TcM TH.Type
annotThType _ _ th_ty@TH.SigT {} = return th_ty
annotThType True ty th_ty
  | not $ isEmptyVarSet $ filterVarSet isTyVar $ tyCoVarsOfType ty =
    do
      let ki = typeKind ty
      th_ki <- reifyKind ki
      return (TH.SigT th_ty th_ki)
annotThType _ _ th_ty = return th_ty

mkIsPolyTvs :: [TyVar] -> [Bool]
mkIsPolyTvs = map is_poly_tv
  where
    is_poly_tv tv =
      not $ isEmptyVarSet $ filterVarSet isTyVar $ tyCoVarsOfType $ tyVarKind tv

reifyClassInstances :: Class -> [ClsInst] -> TcM [TH.Dec]
reifyClassInstances cls = mapM (reifyClassInstance (mkIsPolyTvs tvs))
  where
    tvs = tyConVisibleTyVars (classTyCon cls)

reifyClassInstance :: [Bool] -> ClsInst -> TcM TH.Dec
reifyClassInstance is_poly_tvs i = do
  cxt <- reifyCxt theta
  let vis_types = filterOutInvisibleTypes cls_tc types
  thtypes <- reifyTypes vis_types
  annot_thtypes <- zipWith3M annotThType is_poly_tvs vis_types thtypes
  let head_ty = mkThAppTs (TH.ConT (reifyName cls)) annot_thtypes
  return $ TH.InstanceD over cxt head_ty []
  where
    (_tvs, theta, cls, types) = tcSplitDFunTy (idType dfun)
    cls_tc = classTyCon cls
    dfun = instanceDFunId i
    over = case overlapMode (is_flag i) of
      NoOverlap _ -> Nothing
      Overlappable _ -> Just TH.Overlappable
      Overlapping _ -> Just TH.Overlapping
      Overlaps _ -> Just TH.Overlaps
      Incoherent _ -> Just TH.Incoherent

reifyFamilyInstances :: TyCon -> [FamInst] -> TcM [TH.Dec]
reifyFamilyInstances fam_tc = mapM (reifyFamilyInstance (mkIsPolyTvs fam_tvs))
  where
    fam_tvs = tyConVisibleTyVars fam_tc

reifyFamilyInstance :: [Bool] -> FamInst -> TcM TH.Dec
reifyFamilyInstance is_poly_tvs inst@FamInst {fi_flavor = flavor, fi_fam = fam, fi_tvs = fam_tvs, fi_tys = lhs, fi_rhs = rhs} =
  case flavor of
    SynFamilyInst -> do
      let lhs_types_only = filterOutInvisibleTypes fam_tc lhs
      th_lhs <- reifyTypes lhs_types_only
      annot_th_lhs <- zipWith3M annotThType is_poly_tvs lhs_types_only th_lhs
      th_rhs <- reifyType rhs
      return (TH.TySynInstD (reifyName fam) (TH.TySynEqn annot_th_lhs th_rhs))
    DataFamilyInst rep_tc -> do
      let rep_tvs = tyConTyVars rep_tc
          fam' = reifyName fam
          (_rep_tc, rep_tc_args) = splitTyConApp rhs
          etad_tyvars = dropList rep_tc_args rep_tvs
          etad_tys = mkTyVarTys etad_tyvars
          eta_expanded_tvs = mkTyVarTys fam_tvs `chkAppend` etad_tys
          eta_expanded_lhs = lhs `chkAppend` etad_tys
          dataCons = tyConDataCons rep_tc
          isGadt = isGadtSyntaxTyCon rep_tc
      cons <- mapM (reifyDataCon isGadt eta_expanded_tvs) dataCons
      let types_only = filterOutInvisibleTypes fam_tc eta_expanded_lhs
      th_tys <- reifyTypes types_only
      annot_th_tys <- zipWith3M annotThType is_poly_tvs types_only th_tys
      return
        $ if isNewTyCon rep_tc
        then TH.NewtypeInstD [] fam' annot_th_tys Nothing (head cons) []
        else TH.DataInstD [] fam' annot_th_tys Nothing cons []
  where
    fam_tc = famInstTyCon inst

reifyType :: TyCoRep.Type -> TcM TH.Type
reifyType ty | tcIsLiftedTypeKind ty = return TH.StarT
reifyType ty@ForAllTy {} = reifyForAll ty
reifyType (LitTy t) = do
  r <- reifyTyLit t
  return (TH.LitT r)
reifyType (TyVarTy tv) = return (TH.VarT (reifyName tv))
reifyType (TyConApp tc tys) = reifyTcApp tc tys
reifyType (AppTy t1 t2) = do
  [r1, r2] <- reifyTypes [t1, t2]
  return (r1 `TH.AppT` r2)
reifyType ty@(FunTy t1 t2)
  | isPredTy t1 = reifyForAll ty
  | otherwise =
    do
      [r1, r2] <- reifyTypes [t1, t2]
      return (TH.ArrowT `TH.AppT` r1 `TH.AppT` r2)
reifyType (CastTy t _) = reifyType t
reifyType ty@CoercionTy {} = noTH (sLit "coercions in types") (ppr ty)

reifyForAll :: TyCoRep.Type -> TcM TH.Type
reifyForAll ty = do
  cxt' <- reifyCxt cxt
  tau' <- reifyType tau
  tvs' <- reifyTyVars tvs
  return (TH.ForallT tvs' cxt' tau')
  where
    (tvs, cxt, tau) = tcSplitSigmaTy ty

reifyTyLit :: TyCoRep.TyLit -> TcM TH.TyLit
reifyTyLit (NumTyLit n) = return (TH.NumTyLit n)
reifyTyLit (StrTyLit s) = return (TH.StrTyLit (unpackFS s))

reifyTypes :: [Type] -> TcM [TH.Type]
reifyTypes = mapM reifyType

reifyPatSynType
  :: ([TyVar], ThetaType, [TyVar], ThetaType, [Type], Type) -> TcM TH.Type
reifyPatSynType (univTyVars, req, exTyVars, prov, argTys, resTy) = do
  univTyVars' <- reifyTyVars univTyVars
  req' <- reifyCxt req
  exTyVars' <- reifyTyVars exTyVars
  prov' <- reifyCxt prov
  tau' <- reifyType (mkFunTys argTys resTy)
  return $ TH.ForallT univTyVars' req' $ TH.ForallT exTyVars' prov' tau'

reifyKind :: Kind -> TcM TH.Kind
reifyKind = reifyType

reifyCxt :: [PredType] -> TcM [TH.Pred]
reifyCxt = mapM reifyPred

reifyFunDep :: ([TyVar], [TyVar]) -> TH.FunDep
reifyFunDep (xs, ys) = TH.FunDep (map reifyName xs) (map reifyName ys)

reifyTyVars :: [TyVar] -> TcM [TH.TyVarBndr]
reifyTyVars = mapM reify_tv
  where
    reify_tv tv = TH.KindedTV name <$> reifyKind kind
      where
        kind = tyVarKind tv
        name = reifyName tv

reifyTcApp :: TyCon -> [Type.Type] -> TcM TH.Type
reifyTcApp tc tys = do
  tys' <- reifyTypes (filterOutInvisibleTypes tc tys)
  maybe_sig_t (mkThAppTs r_tc tys')
  where
    arity = tyConArity tc
    tc_binders = tyConBinders tc
    tc_res_kind = tyConResKind tc
    r_tc
      | isUnboxedSumTyCon tc = TH.UnboxedSumT (arity `div` 2)
      | isUnboxedTupleTyCon tc = TH.UnboxedTupleT (arity `div` 2)
      | isPromotedTupleTyCon tc = TH.PromotedTupleT (arity `div` 2)
      | isTupleTyCon tc =
        if isPromotedDataCon tc
        then TH.PromotedTupleT arity
        else TH.TupleT arity
      | tc `hasKey` constraintKindTyConKey = TH.ConstraintT
      | tc `hasKey` funTyConKey = TH.ArrowT
      | tc `hasKey` listTyConKey = TH.ListT
      | tc `hasKey` nilDataConKey = TH.PromotedNilT
      | tc `hasKey` consDataConKey = TH.PromotedConsT
      | tc `hasKey` heqTyConKey = TH.EqualityT
      | tc `hasKey` eqPrimTyConKey = TH.EqualityT
      | tc `hasKey` eqReprPrimTyConKey = TH.ConT (reifyName coercibleTyCon)
      | isPromotedDataCon tc = TH.PromotedT (reifyName tc)
      | otherwise = TH.ConT (reifyName tc)
    maybe_sig_t th_type
      | needs_kind_sig =
        do
          let full_kind = typeKind (mkTyConApp tc tys)
          th_full_kind <- reifyKind full_kind
          return (TH.SigT th_type th_full_kind)
      | otherwise = return th_type
    needs_kind_sig
      | GT <- compareLength tys tc_binders =
        False
      | otherwise =
        let (dropped_binders, remaining_binders) = splitAtList tys tc_binders
            result_kind = mkTyConKind remaining_binders tc_res_kind
            result_vars = tyCoVarsOfType result_kind
            dropped_vars =
              fvVarSet $ mapUnionFV injectiveVarsOfBinder dropped_binders
         in not (subVarSet result_vars dropped_vars)

reifyPred :: TyCoRep.PredType -> TcM TH.Pred
reifyPred ty
  | isIPPred ty = noTH (sLit "implicit parameters") (ppr ty)
  | otherwise = reifyType ty

reifyName :: NamedThing n => n -> TH.Name
reifyName thing
  | isExternalName name = mk_varg pkg_str mod_str occ_str
  | otherwise = TH.mkNameU occ_str (getKey (getUnique name))
  where
    name = getName thing
    mod = nameModule name
    pkg_str = unitIdString (moduleUnitId mod)
    mod_str = moduleNameString (moduleName mod)
    occ_str = occNameString occ
    occ = nameOccName name
    mk_varg
      | OccName.isDataOcc occ = TH.mkNameG_d
      | OccName.isVarOcc occ = TH.mkNameG_v
      | OccName.isTcOcc occ = TH.mkNameG_tc
      | otherwise = pprPanic "reifyName" (ppr name)

reifyFieldLabel :: FieldLabel -> TH.Name
reifyFieldLabel fl
  | flIsOverloaded fl =
    TH.Name (TH.mkOccName occ_str)
      (TH.NameQ (TH.mkModName mod_str))
  | otherwise = TH.mkNameG_v pkg_str mod_str occ_str
  where
    name = flSelector fl
    mod = nameModule name
    pkg_str = unitIdString (moduleUnitId mod)
    mod_str = moduleNameString (moduleName mod)
    occ_str = unpackFS (flLabel fl)

reifySelector :: Id -> TyCon -> TH.Name
reifySelector id tc =
  case find ((idName id ==) . flSelector) (tyConFieldLabels tc) of
    Just fl -> reifyFieldLabel fl
    Nothing -> pprPanic "reifySelector: missing field" (ppr id $$ ppr tc)

reifyFixity :: Name -> TcM (Maybe TH.Fixity)
reifyFixity name = do
  (found, fix) <- lookupFixityRn_help name
  return (if found then Just (conv_fix fix) else Nothing)
  where
    conv_fix (BasicTypes.Fixity _ i d) = TH.Fixity i (conv_dir d)
    conv_dir BasicTypes.InfixR = TH.InfixR
    conv_dir BasicTypes.InfixL = TH.InfixL
    conv_dir BasicTypes.InfixN = TH.InfixN

reifyUnpackedness :: DataCon.SrcUnpackedness -> TH.SourceUnpackedness
reifyUnpackedness NoSrcUnpack = TH.NoSourceUnpackedness
reifyUnpackedness SrcNoUnpack = TH.SourceNoUnpack
reifyUnpackedness SrcUnpack = TH.SourceUnpack

reifyStrictness :: DataCon.SrcStrictness -> TH.SourceStrictness
reifyStrictness NoSrcStrict = TH.NoSourceStrictness
reifyStrictness SrcStrict = TH.SourceStrict
reifyStrictness SrcLazy = TH.SourceLazy

reifySourceBang
  :: DataCon.HsSrcBang -> (TH.SourceUnpackedness, TH.SourceStrictness)
reifySourceBang (HsSrcBang _ u s) = (reifyUnpackedness u, reifyStrictness s)

reifyDecidedStrictness :: DataCon.HsImplBang -> TH.DecidedStrictness
reifyDecidedStrictness HsLazy = TH.DecidedLazy
reifyDecidedStrictness HsStrict = TH.DecidedStrict
reifyDecidedStrictness HsUnpack {} = TH.DecidedUnpack

lookupThAnnLookup :: TH.AnnLookup -> TcM CoreAnnTarget
lookupThAnnLookup (TH.AnnLookupName th_nm) =
  fmap NamedTarget (lookupThName th_nm)
lookupThAnnLookup (TH.AnnLookupModule (TH.Module pn mn)) =
  return $ ModuleTarget
    $ mkModule (stringToUnitId $ TH.pkgString pn)
        (mkModuleName $ TH.modString mn)

reifyAnnotations :: Data a => TH.AnnLookup -> TcM [a]
reifyAnnotations th_name = do
  name <- lookupThAnnLookup th_name
  topEnv <- getTopEnv
  epsHptAnns <- liftIO $ prepareAnnotations topEnv Nothing
  tcg <- getGblEnv
  let selectedEpsHptAnns = findAnns deserializeWithData epsHptAnns name
  let selectedTcgAnns = findAnns deserializeWithData (tcg_ann_env tcg) name
  return (selectedEpsHptAnns ++ selectedTcgAnns)

modToTHMod :: Module -> TH.Module
modToTHMod m =
  TH.Module (TH.PkgName $ unitIdString $ moduleUnitId m)
    (TH.ModName $ moduleNameString $ moduleName m)

reifyModule :: TH.Module -> TcM TH.ModuleInfo
reifyModule (TH.Module (TH.PkgName pkgString) (TH.ModName mString)) = do
  this_mod <- getModule
  let reifMod = mkModule (stringToUnitId pkgString) (mkModuleName mString)
  if reifMod == this_mod then reifyThisModule else reifyFromIface reifMod
  where
    reifyThisModule = do
      usages <- fmap (map modToTHMod . moduleEnvKeys . imp_mods) getImports
      return $ TH.ModuleInfo usages
    reifyFromIface reifMod = do
      iface <-
        loadInterfaceForModule
          (text "reifying module from TH for" <+> ppr reifMod)
          reifMod
      let usages =
            [ modToTHMod m
              | usage <- mi_usages iface,
                Just m <- [usageToModule (moduleUnitId reifMod) usage]
              ]
      return $ TH.ModuleInfo usages
    usageToModule :: UnitId -> Usage -> Maybe Module
    usageToModule _ UsageFile {} = Nothing
    usageToModule this_pkg UsageHomeModule {usg_mod_name = mn} =
      Just $ mkModule this_pkg mn
    usageToModule _ UsagePackageModule {usg_mod = m} = Just m
    usageToModule _ UsageMergedRequirement {usg_mod = m} = Just m

mkThAppTs :: TH.Type -> [TH.Type] -> TH.Type
mkThAppTs = foldl TH.AppT

noTH :: LitString -> SDoc -> TcM a
noTH s d =
  failWithTc
    ( hsep
        [ text "Can't represent" <+> ptext s <+> text "in Template Haskell:",
          nest 2 d
          ]
      )

pprTh :: TH.Ppr a => a -> SDoc
pprTh x = text (TH.pprint x)
