{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Asterius.GHCi.Internals
  ( asteriusStartIServ,
    asteriusStopIServ,
    asteriusIservCall,
    asteriusHscCompileCoreExpr
    )
where

import Asterius.CodeGen
import Asterius.Types
import Asterius.TypesConv
import qualified CLabel as GHC
import qualified CmmInfo as GHC
import Control.Exception
import qualified CoreLint as GHC
import qualified CorePrep as GHC
import qualified CoreTidy as GHC
import qualified CoreToStg as GHC
import qualified CostCentre as GHC
import Data.Binary
import Data.Data
  ( Data,
    gmapQl
    )
import Data.IORef
import Data.String
import qualified GHCi.Message as GHC
import qualified GHCi.RemoteTypes as GHC
import qualified GhcPlugins as GHC
import qualified HscMain as GHC
import qualified Linker as GHC
import qualified Panic as GHC
import qualified SimplCore as GHC
import qualified SimplStg as GHC
import qualified Stream
import System.IO.Unsafe
import Type.Reflection
import qualified UniqDSet as GHC

newtype GHCiState = GHCiState {ghciUniqSupply :: GHC.UniqSupply}

{-# NOINLINE globalGHCiState #-}
globalGHCiState :: IORef GHCiState
globalGHCiState = unsafePerformIO $ do
  us <- GHC.mkSplitUniqSupply 'A'
  newIORef GHCiState {ghciUniqSupply = us}

asteriusStartIServ :: GHC.DynFlags -> IO GHC.IServ
asteriusStartIServ _ = do
  cache_ref <- newIORef GHC.emptyUFM
  pure GHC.IServ
    { GHC.iservPipe = error "asteriusStartIServ.iservPipe",
      GHC.iservProcess = error "asteriusStartIServ.iservProcess",
      GHC.iservLookupSymbolCache = cache_ref,
      GHC.iservPendingFrees = []
      }

asteriusStopIServ :: GHC.HscEnv -> IO ()
asteriusStopIServ _ = pure ()

asteriusIservCall :: Binary a => GHC.IServ -> GHC.Message a -> IO a
asteriusIservCall _ msg = do
  putStrLn $ "[INFO] " <> show msg
  case msg of
    GHC.InitLinker -> pure ()
    GHC.LoadDLL _ -> pure Nothing
    GHC.LoadArchive _ -> pure ()
    GHC.AddLibrarySearchPath _ -> pure $ GHC.RemotePtr 0
    GHC.RemoveLibrarySearchPath _ -> pure True
    GHC.ResolveObjs -> pure True
    GHC.FindSystemLibrary lib -> pure $ Just lib
    GHC.StartTH -> newIORef (error "asteriusIservCall.StartTH") >>= GHC.mkRemoteRef
    _ -> fail "asteriusIservCall"

asteriusHscCompileCoreExpr
  :: GHC.HscEnv -> GHC.SrcSpan -> GHC.CoreExpr -> IO GHC.ForeignHValue
asteriusHscCompileCoreExpr hsc_env srcspan ds_expr = do
  let dflags = GHC.hsc_dflags hsc_env
  simpl_expr <- GHC.simplifyExpr dflags ds_expr
  let tidy_expr = GHC.tidyExpr GHC.emptyTidyEnv simpl_expr
  prepd_expr <- GHC.corePrepExpr dflags hsc_env tidy_expr
  GHC.lintInteractiveExpr "asteriusHscCompileCoreExpr" hsc_env prepd_expr
  linkGhci hsc_env
  asteriusLinkExpr hsc_env srcspan prepd_expr
  u <-
    atomicModifyIORef' globalGHCiState $ \s ->
      let (u, s') = GHC.takeUniqFromSupply $ ghciUniqSupply s
       in (s {ghciUniqSupply = s'}, u)
  let this_mod =
        GHC.mkModule (GHC.stringToUnitId "asdf")
          (GHC.mkModuleName $ "ASDF" <> show u)
      occ_n = GHC.mkVarOcc "asdf"
      n = GHC.mkExternalName u this_mod occ_n srcspan
      clbl = GHC.mkClosureLabel n GHC.MayHaveCafRefs
      sym = fromString $ asmPpr dflags clbl :: AsteriusEntitySymbol
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
  GHC.mkRemoteRef (error "asteriusHscCompileCoreExpr.mkRemoteRef") >>= flip GHC.mkForeignRef (pure ())

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
      | Just HRefl <- eqTypeRep (typeOf t) (typeRep @GHC.Name) =
        case GHC.nameModule_maybe t of
          Just m
            | GHC.isExternalName t && not (GHC.isWiredInName t) ->
              GHC.unitUniqDSet m
          _ -> GHC.emptyUniqDSet
      | otherwise =
        gmapQl GHC.unionUniqDSets GHC.emptyUniqDSet w t

linkGhci :: GHC.HscEnv -> IO ()
linkGhci hsc_env =
  GHC.linkPackages
    hsc_env
    [GHC.componentIdToInstalledUnitId ghci_comp_id]
  where
    Just ghci_comp_id =
      GHC.lookupPackageName (GHC.hsc_dflags hsc_env) (GHC.PackageName "ghci")
