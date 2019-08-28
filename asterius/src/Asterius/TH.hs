{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Asterius.TH
  ( asteriusRunMeta
    )
where

import Annotations
import Bag
import Control.Exception
import Convert
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import Data.Typeable (TypeRep)
import DsExpr
import DsMonad
import DynFlags
import ErrUtils
import GHC.Serialized
import GHCi
import GHCi.Message
import GHCi.RemoteTypes
import HsSyn
import HscMain
import HscTypes
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import Module
import Name
import Outputable
import Panic
import Plugins
import RdrName
import RnEnv
import SrcLoc
import TcRnMonad
import TcSplice ()
import Type.Reflection (Typeable)
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
    liftIO $ withForeignRef rstate $ \state_hv -> withForeignRef fhv $ \q_hv ->
      writeIServ (hsc_dflags hsc_env) i (RunTH state_hv q_hv ty (Just loc))
    runRemoteTH (hsc_dflags hsc_env) i []
    bs <- readQResult (hsc_dflags hsc_env) i
    return $! runGet get (LB.fromStrict bs)

runRemoteTH :: DynFlags -> IServ -> [Messages] -> TcM ()
runRemoteTH dflags iserv recovers = do
  THMsg msg <- liftIO $ readIServ dflags iserv
  case msg of
    RunTHDone -> return ()
    StartRecover -> do
      v <- getErrsVar
      msgs <- readTcRef v
      writeTcRef v emptyMessages
      runRemoteTH dflags iserv (msgs : recovers)
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
      runRemoteTH dflags iserv rest
    _other -> do
      r <- handleTHMessage msg
      liftIO $ writeIServ dflags iserv r
      runRemoteTH dflags iserv recovers

readQResult :: (Binary a, Typeable a) => DynFlags -> IServ -> TcM a
readQResult dflags i = do
  qr <- liftIO $ readIServ dflags i
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

notInScope :: TH.Name -> SDoc
notInScope th_name =
  quotes (text (TH.pprint th_name)) <+> text "is not in scope at a reify"

lookupThAnnLookup :: TH.AnnLookup -> TcM CoreAnnTarget
lookupThAnnLookup (TH.AnnLookupName th_nm) =
  fmap NamedTarget (lookupThName th_nm)
lookupThAnnLookup (TH.AnnLookupModule (TH.Module pn mn)) =
  return $ ModuleTarget
    $ mkModule (stringToUnitId $ TH.pkgString pn)
        (mkModuleName $ TH.modString mn)
