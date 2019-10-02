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
    asteriusHscCompileCoreExpr,
  )
where

import Asterius.CodeGen
import Asterius.Internals ((!))
import Asterius.Internals.Name
import Asterius.Internals.Temp
import Asterius.JSRun.Main
import Asterius.JSRun.NonMain
import Asterius.Ld
import Asterius.Resolve
import qualified Asterius.Types
import Asterius.Types
import Asterius.TypesConv
import qualified BasicTypes as GHC
import qualified CLabel as GHC
import qualified CmmInfo as GHC
import Control.Concurrent
import Control.Exception
import qualified CoreLint as GHC
import qualified CorePrep as GHC
import qualified CoreSyn as GHC
import qualified CoreTidy as GHC
import qualified CoreToStg as GHC
import qualified CoreUtils as GHC
import qualified CostCentre as GHC
import Data.Binary
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import Data.Data
  ( Data,
    gmapQl,
  )
import Data.IORef
import Data.String
import qualified ErrUtils as GHC
import Foreign.ForeignPtr
import Foreign.Ptr
import GHC.IO.Handle.FD
import qualified GHCi.Message as GHC
import GHCi.Message
import qualified GHCi.RemoteTypes as GHC
import qualified HscMain as GHC
import qualified HscTypes as GHC
import qualified Id as GHC
import qualified IdInfo as GHC
import Language.Haskell.TH
import Language.JavaScript.Inline.Core
import qualified Linker as GHC
import qualified Module as GHC
import qualified Name as GHC
import qualified Outputable as GHC
import qualified Packages as GHC
import qualified Panic as GHC
import qualified SimplCore as GHC
import qualified SimplStg as GHC
import qualified SrcLoc as GHC
import qualified Stream
import System.Directory
import System.Environment.Blank
import System.FilePath
import System.IO
import System.IO.Unsafe
import System.Process
import Type.Reflection
import qualified UniqDSet as GHC
import qualified UniqFM as GHC
import qualified UniqSupply as GHC
import Unsafe.Coerce
import qualified VarEnv as GHC

data GHCiState = GHCiState {ghciUniqSupply :: !GHC.UniqSupply, ghciLibs, ghciObjs :: ![FilePath], ghciJSSession :: !(Maybe (JSSession, Pipe, JSVal)), ghciTemp :: (Asterius.Types.Module, LinkReport)}

{-# NOINLINE globalGHCiState #-}
globalGHCiState :: MVar GHCiState
globalGHCiState = unsafePerformIO $ do
  us <- GHC.mkSplitUniqSupply 'A'
  newMVar GHCiState
    { ghciUniqSupply = us,
      ghciLibs = [],
      ghciObjs = [],
      ghciJSSession = Nothing,
      ghciTemp = error "Temp not ready yet"
    }

asteriusStartIServ :: GHC.HscEnv -> IO GHC.IServ
asteriusStartIServ hsc_env = do
  GHC.debugTraceMsg (GHC.hsc_dflags hsc_env) 3 $ GHC.text "asteriusStartIServ"
  (node_read_fd, host_write_fd) <- createPipeFd
  (host_read_fd, node_write_fd) <- createPipeFd
  setEnv "ASTERIUS_NODE_READ_FD" (show node_read_fd) True
  setEnv "ASTERIUS_NODE_WRITE_FD" (show node_write_fd) True
  host_read_handle <- fdToHandle host_read_fd
  host_write_handle <- fdToHandle host_write_fd
  hSetBuffering host_read_handle NoBuffering
  hSetBuffering host_write_handle NoBuffering
  lo_ref <- newIORef Nothing
  let p = Pipe
        { pipeRead = host_read_handle,
          pipeWrite = host_write_handle,
          pipeLeftovers = lo_ref
        }
  s <- newJSSession defJSSessionOpts {nodeStdErrInherit = True}
  modifyMVar_ globalGHCiState $ \st ->
    pure
      st
        { ghciJSSession =
            Just
              (s, p, error "JSVal of asterius instance uninitialized")
        }
  cache_ref <- newIORef GHC.emptyUFM
  pure GHC.IServ
    { GHC.iservPipe = error "asteriusStartIServ.iservPipe",
      GHC.iservProcess = error "asteriusStartIServ.iservProcess",
      GHC.iservLookupSymbolCache = cache_ref,
      GHC.iservPendingFrees = []
    }

asteriusStopIServ :: GHC.HscEnv -> IO ()
asteriusStopIServ hsc_env = do
  GHC.debugTraceMsg (GHC.hsc_dflags hsc_env) 3 $ GHC.text "asteriusStopIServ"
  modifyMVar_ globalGHCiState $ \st -> case ghciJSSession st of
    Just (s, _, _) -> do
      closeJSSession s
      pure st {ghciJSSession = Nothing}
    _ -> pure st
  pure ()

asteriusIservCall ::
  Binary a => GHC.HscEnv -> GHC.IServ -> GHC.Message a -> IO a
asteriusIservCall hsc_env _ msg = do
  GHC.debugTraceMsg (GHC.hsc_dflags hsc_env) 3 $ GHC.text $ show msg
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

asteriusReadIServ ::
  forall a. (Binary a, Typeable a) => GHC.HscEnv -> GHC.IServ -> IO a
asteriusReadIServ _ _ = withMVar globalGHCiState
  $ \s -> let Just (_, p, _) = ghciJSSession s in readPipe p get

asteriusWriteIServ ::
  (Binary a, Typeable a) => GHC.HscEnv -> GHC.IServ -> a -> IO ()
asteriusWriteIServ hsc_env i a
  | Just HRefl <- eqTypeRep (typeOf a) (typeRep @GHC.Msg) = case a of
    GHC.Msg m@(GHC.RunTH st q ty loc) -> do
      GHC.debugTraceMsg (GHC.hsc_dflags hsc_env) 3 $ GHC.text $ show m
      modifyMVar_ globalGHCiState $ \s -> do
        js_s <-
          maybe (fail "asteriusWriteIServ: RunTH: no JSSession") pure
            $ (\(x, _, _) -> x)
              <$> ghciJSSession s
        v <- asteriusRunTH hsc_env i st q ty loc js_s (ghciTemp s)
        pure
          s
            { ghciJSSession =
                let Just (x, y, _) = ghciJSSession s
                 in Just (x, y, v),
              ghciTemp = error "Temp cleaned"
            }
    GHC.Msg m@GHC.RunModFinalizers {} -> do
      GHC.debugTraceMsg (GHC.hsc_dflags hsc_env) 3 $ GHC.text $ show m
      withMVar globalGHCiState $ \s -> do
        (js_s, v) <-
          maybe (fail "asteriusWriteIServ: RunTH: no JSSession") pure
            $ (\(x, _, v) -> (x, v))
              <$> ghciJSSession s
        asteriusRunModFinalizers hsc_env js_s v
    GHC.Msg m -> fail $ "asteriusWriteIServ: unsupported message " <> show m
  | otherwise = withMVar globalGHCiState
    $ \s -> let Just (_, p, _) = ghciJSSession s in writePipe p $ put a

asteriusRunTH ::
  GHC.HscEnv ->
  GHC.IServ ->
  GHC.RemoteRef (IORef GHC.QState) ->
  GHC.HValueRef ->
  GHC.THResultType ->
  Maybe Loc ->
  JSSession ->
  (Asterius.Types.Module, LinkReport) ->
  IO JSVal
asteriusRunTH hsc_env _ _ q ty _ s ahc_dist_input =
  withTempDir "asdf" $ \tmp_dir -> do
    let p = tmp_dir </> "asdf"
    distNonMain p [run_q_sym, run_mod_fin_sym] ahc_dist_input
    mod_buf <- LBS.readFile $ p -<.> "wasm"
    lib_val <- importMJS s $ p -<.> "lib.mjs"
    f_val <- eval s $ takeJSVal lib_val <> ".default"
    buf_val <- alloc s mod_buf
    mod_val <- eval s $ "WebAssembly.compile(" <> takeJSVal buf_val <> ")"
    i <- eval s $ takeJSVal f_val <> "(" <> takeJSVal mod_val <> ")"
    hsInit s i
    let run_q_exp_closure =
          deRefJSVal i <> ".symbolTable."
            <> coerce
                 (shortByteString (coerce run_q_sym))
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
          deRefJSVal i <> ".exports.rts_evalLazyIO(" <> applied_closure <> ")"
    (_ :: IO ()) <- eval' s tid
    pure i
  where
    run_q_sym = ghciClosureSymbol hsc_env "Asterius.GHCi" $ case ty of
      GHC.THExp -> "asteriusRunQExp"
      GHC.THPat -> "asteriusRunQPat"
      GHC.THType -> "asteriusRunQType"
      GHC.THDec -> "asteriusRunQDec"
      _ -> error $ "asteriusRunTH: unsupported THResultType " <> show ty
    run_mod_fin_sym =
      ghciClosureSymbol hsc_env "Asterius.GHCi" "asteriusRunModFinalizers"

asteriusRunModFinalizers :: GHC.HscEnv -> JSSession -> JSVal -> IO ()
asteriusRunModFinalizers hsc_env s i = do
  let run_mod_fin_closure =
        deRefJSVal i <> ".symbolTable."
          <> coerce
               (shortByteString (coerce run_mod_fin_sym))
      tid =
        deRefJSVal i <> ".exports.rts_evalLazyIO(" <> run_mod_fin_closure <> ")"
  (_ :: IO ()) <- eval' s tid
  pure ()
  where
    run_mod_fin_sym =
      ghciClosureSymbol hsc_env "Asterius.GHCi" "asteriusRunModFinalizers"

asteriusHscCompileCoreExpr ::
  GHC.HscEnv -> GHC.SrcSpan -> GHC.CoreExpr -> IO GHC.ForeignHValue
asteriusHscCompileCoreExpr hsc_env srcspan ds_expr = do
  let dflags = GHC.hsc_dflags hsc_env
      run_q_exp_sym = ghciClosureSymbol hsc_env "Asterius.GHCi" "asteriusRunQExp"
      run_q_pat_sym = ghciClosureSymbol hsc_env "Asterius.GHCi" "asteriusRunQPat"
      run_q_type_sym =
        ghciClosureSymbol hsc_env "Asterius.GHCi" "asteriusRunQType"
      run_q_dec_sym = ghciClosureSymbol hsc_env "Asterius.GHCi" "asteriusRunQDec"
      run_mod_fin_sym =
        ghciClosureSymbol hsc_env "Asterius.GHCi" "asteriusRunModFinalizers"
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
            rootSymbols =
              [ run_q_exp_sym,
                run_q_pat_sym,
                run_q_type_sym,
                run_q_dec_sym,
                run_mod_fin_sym,
                sym
              ],
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

ghciClosureSymbol :: GHC.HscEnv -> String -> String -> AsteriusEntitySymbol
ghciClosureSymbol hsc_env = fakeClosureSymbol (GHC.hsc_dflags hsc_env) "ghci"
