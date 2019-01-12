{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Toolkit.Run
  ( Config
  , ghcFlags
  , ghcLibDir
  , compiler
  , defaultConfig
  , runHaskell
  , runCmm
  ) where

import Config
import Control.Monad.IO.Class
import Data.Functor
import Data.IORef
import qualified Data.Map.Strict as M
import DriverPhases
import DriverPipeline
import DynFlags
import GHC
import qualified Language.Haskell.GHC.Toolkit.BuildInfo as BI
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.Hooks
import Panic

data Config = Config
  { ghcFlags :: [String]
  , ghcLibDir :: FilePath
  , compiler :: Compiler
  }

defaultConfig :: IO Config
defaultConfig = do
  ghcLibDir <- BI.getGhcLibDir
  return $ Config
    {ghcFlags = ["-Wall", "-O"], ghcLibDir = ghcLibDir, compiler = mempty}

runHaskell ::
     Config -> [String] -> (M.Map Module HaskellIR -> GHC.Ghc r) -> GHC.Ghc r
runHaskell Config {..} targets cont = do
  dflags <- getSessionDynFlags
  (dflags', _, _) <-
    parseDynamicFlags
      (dflags `gopt_set` Opt_ForceRecomp `gopt_unset` Opt_KeepHiFiles `gopt_unset`
       Opt_KeepOFiles) $
    map noLoc ghcFlags
  (h, read_mod_map) <-
    liftIO $ do
      mod_map_ref <- newIORef M.empty
      h <-
        hooksFromCompiler
          (compiler <>
           mempty
             { withHaskellIR =
                 \ModSummary {..} ir ->
                   liftIO $
                   atomicModifyIORef' mod_map_ref $ \mod_map ->
                     (M.insert ms_mod ir mod_map, ())
             })
      pure (h, liftIO $ readIORef mod_map_ref)
  void $
    setSessionDynFlags
      dflags'
        { ghcMode = CompManager
        , ghcLink = NoLink
        , integerLibrary = IntegerSimple
        , tablesNextToCode = False
        , hooks = h
        }
  traverse (`guessTarget` Nothing) targets >>= setTargets
  ok_flag <- load LoadAllTargets
  case ok_flag of
    Succeeded -> liftIO read_mod_map >>= cont
    Failed -> liftIO $ throwGhcExceptionIO $ Panic "GHC.load returned Failed."

runCmm ::
     Config -> [FilePath] -> (M.Map FilePath CmmIR -> GHC.Ghc r) -> GHC.Ghc r
runCmm Config {..} cmm_fns cont = do
  liftIO $ putStrLn $ "runCmm on " ++ show cmm_fns
  dflags <- getSessionDynFlags
  (dflags', _, _) <-
    parseDynamicFlags
      ((dflags `gopt_set` Opt_ForceRecomp `gopt_unset` Opt_KeepOFiles `gopt_set` Opt_KeepTmpFiles) { verbosity = 3 })$
    map noLoc ghcFlags
  (h, read_cmm_irs) <-
    liftIO $ do
      cmm_irs_ref <- newIORef []
      h <-
        hooksFromCompiler
          (compiler <>
           mempty {withCmmIR = \ir -> liftIO $ modifyIORef' cmm_irs_ref (ir :)})
      pure (h, reverse <$> readIORef cmm_irs_ref)
  void $
    setSessionDynFlags
      dflags'
        { ghcMode = OneShot
        , ghcLink = NoLink
        , integerLibrary = IntegerSimple
        , tablesNextToCode = False
        , hooks = h
        }
  env <- getSession
  liftIO
    (do putStrLn $ "Running oneShot"
        sequence_ [oneShot env StopLn [(cmm_fn, Just CmmCpp)] | cmm_fn <- cmm_fns]
        putStrLn $ "Done running oneShot"
        cmm_irs <- read_cmm_irs
        if length cmm_irs == length cmm_fns
          then pure $ M.fromList $ zip cmm_fns cmm_irs
          else throwGhcExceptionIO $ Panic "Unknown error when compiling .cmm") >>=
    cont
