{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Asterius.GHCi.Internals
  ( asteriusStartIServ,
    asteriusStopIServ,
    asteriusIservCall,
    asteriusReadIServ,
    asteriusWriteIServ,
    asteriusHscCompileCoreExpr
    )
where

import Asterius.CodeGen
import Asterius.Internals ((!))
import Asterius.Internals.Temp
import Asterius.JSRun.Main
import Asterius.JSRun.NonMain
import Asterius.Ld
import Asterius.Resolve
import qualified Asterius.Types
import Asterius.TypesConv
import qualified CLabel as GHC
import qualified CmmInfo as GHC
import Control.Concurrent
import Control.Exception
import qualified CoreLint as GHC
import qualified CorePrep as GHC
import qualified CoreTidy as GHC
import qualified CoreToStg as GHC
import qualified CostCentre as GHC
import Data.Binary
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Data
  ( Data,
    gmapQl
    )
import Data.IORef
import Data.String
import Foreign.ForeignPtr
import Foreign.Ptr
import qualified GHCi.Message as GHC
import qualified GHCi.RemoteTypes as GHC
import qualified GhcPlugins as GHC
import qualified HscMain as GHC
import Language.Haskell.TH
import Language.JavaScript.Inline.Core
import qualified Linker as GHC
import qualified Panic as GHC
import qualified SimplCore as GHC
import qualified SimplStg as GHC
import qualified Stream
import System.Directory
import System.FilePath
import System.IO.Unsafe
import Type.Reflection
import qualified UniqDSet as GHC
import Unsafe.Coerce

data GHCiState = GHCiState {ghciUniqSupply :: !GHC.UniqSupply, ghciLibs, ghciObjs :: ![FilePath], ghciJSSession :: JSSession, ghciQResult :: GHC.QResult BS.ByteString, ghciTemp :: (Asterius.Types.Module, LinkReport)}

{-# NOINLINE globalGHCiState #-}
globalGHCiState :: MVar GHCiState
globalGHCiState = unsafePerformIO $ do
  us <- GHC.mkSplitUniqSupply 'A'
  newMVar GHCiState
    { ghciUniqSupply = us,
      ghciLibs = [],
      ghciObjs = [],
      ghciJSSession = error "JSSession not available yet",
      ghciQResult = error "QResult not ready yet",
      ghciTemp = error "Temp not ready yet"
      }

asteriusStartIServ :: GHC.HscEnv -> IO GHC.IServ
asteriusStartIServ _ = do
  putStrLn "[INFO] startIServ"
  s <- newJSSession defJSSessionOpts {nodeStdErrInherit = True}
  modifyMVar_ globalGHCiState $ \st -> pure st {ghciJSSession = s}
  cache_ref <- newIORef GHC.emptyUFM
  pure GHC.IServ
    { GHC.iservPipe = error "asteriusStartIServ.iservPipe",
      GHC.iservProcess = error "asteriusStartIServ.iservProcess",
      GHC.iservLookupSymbolCache = cache_ref,
      GHC.iservPendingFrees = []
      }

asteriusStopIServ :: GHC.HscEnv -> IO ()
asteriusStopIServ _ = do
  putStrLn "[INFO] stopIServ"
  modifyMVar_ globalGHCiState $ \st -> do
    closeJSSession $ ghciJSSession st
    pure st {ghciJSSession = error "JSSession closed"}
  pure ()

asteriusIservCall
  :: Binary a => GHC.HscEnv -> GHC.IServ -> GHC.Message a -> IO a
asteriusIservCall _ _ msg = do
  putStrLn $ "[INFO] " <> show msg
  case msg of
    GHC.InitLinker -> pure ()
    GHC.LoadDLL _ -> pure Nothing
    GHC.LoadArchive lib ->
      modifyMVar_ globalGHCiState $ \s -> pure s {ghciLibs = lib : ghciLibs s}
    GHC.LoadObj obj -> modifyMVar_ globalGHCiState
      $ \s -> pure s {ghciObjs = obj : ghciObjs s}
    GHC.AddLibrarySearchPath _ -> pure $ GHC.RemotePtr 0
    GHC.RemoveLibrarySearchPath _ -> pure True
    GHC.ResolveObjs -> pure True
    GHC.FindSystemLibrary lib -> pure $ Just lib
    GHC.StartTH -> pure $ unsafeCoerce $ GHC.RemotePtr 233
    _ -> fail "asteriusIservCall"

asteriusReadIServ :: forall a. Typeable a => GHC.HscEnv -> GHC.IServ -> IO a
asteriusReadIServ _ _
  | Just HRefl <- eqTypeRep (typeRep @a) (typeRep @GHC.THMsg) =
    pure $ GHC.THMsg GHC.RunTHDone
  | Just HRefl <- eqTypeRep (typeRep @a) (typeRep @(GHC.QResult BS.ByteString)) =
    modifyMVar globalGHCiState $ \st ->
      pure (st {ghciQResult = error "QResult cleaned"}, ghciQResult st)
  | Just HRefl <- eqTypeRep (typeRep @a) (typeRep @(GHC.QResult ())) =
    pure $ GHC.QDone ()
  | otherwise =
    fail $ "asteriusReadIServ: unsupported type " <> show (typeRep @a)

asteriusWriteIServ :: Typeable a => GHC.HscEnv -> GHC.IServ -> a -> IO ()
asteriusWriteIServ hsc_env i a
  | Just HRefl <-
      eqTypeRep
        (typeOf a)
        (typeRep @(GHC.Message (GHC.QResult BS.ByteString))) =
    case a of
      GHC.RunTH st q ty loc -> do
        putStrLn $ "[INFO] " <> show a
        modifyMVar_ globalGHCiState $ \s -> do
          qr <- asteriusRunTH hsc_env i st q ty loc (ghciJSSession s) (ghciTemp s)
          pure s {ghciQResult = qr, ghciTemp = error "Temp cleaned"}
  | Just HRefl <- eqTypeRep (typeOf a) (typeRep @(GHC.Message (GHC.QResult ()))) =
    case a of
      GHC.RunModFinalizers {} -> do
        putStrLn $ "[INFO] " <> show a
        pure ()
  | otherwise =
    fail $ "asteriusWriteIServ: unsupported type " <> show (typeOf a)

asteriusRunTH
  :: GHC.HscEnv
  -> GHC.IServ
  -> GHC.RemoteRef (IORef GHC.QState)
  -> GHC.HValueRef
  -> GHC.THResultType
  -> Maybe Loc
  -> JSSession
  -> (Asterius.Types.Module, LinkReport)
  -> IO (GHC.QResult BS.ByteString)
asteriusRunTH _ _ _ q ty _ s ahc_dist_input = case ty of
  GHC.THExp -> withTempDir "asdf" $ \tmp_dir -> do
    let p = tmp_dir </> "asdf"
    distNonMain p ["ghci_AsteriusziGHCi_asteriusRunQExp_closure"] ahc_dist_input
    mod_buf <- LBS.readFile $ p -<.> "wasm"
    lib_val <- importMJS s $ p -<.> "lib.mjs"
    f_val <- eval s $ takeJSVal lib_val <> ".default"
    buf_val <- alloc s mod_buf
    mod_val <- eval s $ "WebAssembly.compile(" <> takeJSVal buf_val <> ")"
    i <- eval s $ takeJSVal f_val <> "(" <> takeJSVal mod_val <> ")"
    hsInit s i
    let run_q_exp_closure =
          deRefJSVal i
            <> ".symbolTable.ghci_AsteriusziGHCi_asteriusRunQExp_closure"
        GHC.RemotePtr q_addr = unsafeCoerce q
        q_exp_closure = "0x" <> JSCode (word64Hex q_addr)
        applied_closure =
          deRefJSVal i
            <> ".exports.rts_apply("
            <> run_q_exp_closure
            <> ","
            <> q_exp_closure
            <> ")"
        tid =
          "await "
            <> deRefJSVal i
            <> ".exports.rts_evalIO("
            <> applied_closure
            <> ")"
        ret = deRefJSVal i <> ".exports.getTSOret(" <> tid <> ")"
        sp = deRefJSVal i <> ".exports.rts_getStablePtr(" <> ret <> ")"
        val' = deRefJSVal i <> ".getJSVal(" <> sp <> ")"
        val = "(async () => " <> val' <> ")()"
    result_lbs <- eval s val
    pure $ GHC.QDone $ LBS.toStrict result_lbs
  _ -> fail $ "asteriusRunTH: unsupported THResultType " <> show ty

asteriusHscCompileCoreExpr
  :: GHC.HscEnv -> GHC.SrcSpan -> GHC.CoreExpr -> IO GHC.ForeignHValue
asteriusHscCompileCoreExpr hsc_env srcspan ds_expr = do
  let dflags = GHC.hsc_dflags hsc_env
  simpl_expr <- GHC.simplifyExpr dflags ds_expr
  let tidy_expr = GHC.tidyExpr GHC.emptyTidyEnv simpl_expr
  prepd_expr <- GHC.corePrepExpr dflags hsc_env tidy_expr
  GHC.lintInteractiveExpr "asteriusHscCompileCoreExpr" hsc_env prepd_expr
  linkRts hsc_env
  linkGhci hsc_env
  asteriusLinkExpr hsc_env srcspan prepd_expr
  u <-
    modifyMVar globalGHCiState $ \s ->
      let (u, s') = GHC.takeUniqFromSupply $ ghciUniqSupply s
       in pure (s {ghciUniqSupply = s'}, u)
  let this_mod =
        GHC.mkModule (GHC.stringToUnitId "asdf")
          (GHC.mkModuleName $ "ASDF" <> show u)
      occ_n = GHC.mkVarOcc "asdf"
      n = GHC.mkExternalName u this_mod occ_n srcspan
      clbl = GHC.mkClosureLabel n GHC.MayHaveCafRefs
      sym = fromString $ asmPpr dflags clbl
      b = GHC.mkVanillaGlobal n (GHC.exprType ds_expr)
      prepd_binds = [GHC.NonRec b prepd_expr]
      (stg_binds, _) = GHC.coreToStg dflags this_mod prepd_binds
  stg_binds2 <- GHC.stg2stg dflags stg_binds
  cmms <-
    GHC.doCodeGen hsc_env
      this_mod
      []
      GHC.emptyCollectedCCs
      stg_binds2
      (GHC.emptyHpcInfo False)
  raw_cmms <- GHC.cmmToRawCmm dflags (Just this_mod) cmms >>= Stream.collect
  m <-
    either throwIO pure
      $ runCodeGen (marshalRawCmm this_mod raw_cmms) dflags this_mod
  (_, _, LinkReport {..}) <-
    modifyMVar globalGHCiState $ \s -> do
      r@(_, !final_m, !link_report) <-
        linkExeInMemory LinkTask
          { progName = "",
            linkOutput = "",
            linkObjs = ghciObjs s,
            linkLibs = ghciLibs s,
            linkModule = m,
            debug = False,
            gcSections = True,
            binaryen = False,
            verboseErr = True,
            outputIR = Nothing,
            rootSymbols = ["ghci_AsteriusziGHCi_asteriusRunQExp_closure", sym],
            exportFunctions = []
            }
      pure (s {ghciTemp = (final_m, link_report)}, r)
  fmap unsafeCoerce
    $ newForeignPtr_
    $ wordPtrToPtr
    $ fromIntegral
    $ staticsSymbolMap
    ! sym

asteriusLinkExpr :: GHC.HscEnv -> GHC.SrcSpan -> GHC.CoreExpr -> IO ()
asteriusLinkExpr hsc_env srcspan prepd_expr = do
  GHC.initDynLinker hsc_env
  GHC.modifyPLS $ \pls0 -> do
    (pls, ok) <-
      GHC.linkDependencies hsc_env pls0 srcspan
        $ neededModules prepd_expr
    if GHC.failed ok
      then GHC.throwGhcExceptionIO (GHC.ProgramError "")
      else pure (pls, ())

neededModules :: Data a => a -> [GHC.Module]
neededModules = GHC.uniqDSetToList . w
  where
    w :: Data a => a -> GHC.UniqDSet GHC.Module
    w t
      | Just HRefl <- eqTypeRep (typeOf t) (typeRep @GHC.CoreBndr) =
        let n = GHC.getName t
         in case GHC.nameModule_maybe n of
              Just m
                | GHC.isExternalName n && not (GHC.isWiredInName n) ->
                  GHC.unitUniqDSet m
              _ -> GHC.emptyUniqDSet
      | otherwise =
        gmapQl GHC.unionUniqDSets GHC.emptyUniqDSet w t

linkRts :: GHC.HscEnv -> IO ()
linkRts hsc_env = do
  let Just pkg_cfg =
        GHC.lookupInstalledPackage
          (GHC.hsc_dflags hsc_env)
          (GHC.toInstalledUnitId GHC.rtsUnitId)
  Just rts_path <- findFile (GHC.libraryDirs pkg_cfg) "libHSrts.a"
  asteriusIservCall hsc_env (error "linkRts") $ GHC.LoadArchive rts_path

linkGhci :: GHC.HscEnv -> IO ()
linkGhci hsc_env =
  GHC.linkPackages
    hsc_env
    [GHC.componentIdToInstalledUnitId ghci_comp_id]
  where
    Just ghci_comp_id =
      GHC.lookupPackageName (GHC.hsc_dflags hsc_env) (GHC.PackageName "ghci")
