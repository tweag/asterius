{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Toolkit.Run
  ( Config
  , ghcFlags
  , ghcLibDir
  , defaultConfig
  , runHaskell
  , runCmm
  ) where

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
  }

defaultConfig :: Config
defaultConfig =
  Config
    { ghcFlags =
        [ "-Wall"
        , "-O2"
        , "-fforce-recomp"
        , "-no-keep-hi-files"
        , "-no-keep-o-files"
        ]
    , ghcLibDir = BI.ghcLibDir
    }

runHaskell :: MonadIO m => Config -> [String] -> m (M.Map Module HaskellIR)
runHaskell Config {..} targets =
  liftIO $
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
  runGhc (Just ghcLibDir) $ do
    dflags <- getSessionDynFlags
    (dflags', _, _) <- parseDynamicFlags dflags $ map noLoc ghcFlags
    (h, read_mod_map) <-
      liftIO $ do
        mod_map_ref <- newIORef M.empty
        h <-
          hooksFromCompiler $
          defaultCompiler
            { withHaskellIR =
                \ModSummary {..} ir ->
                  liftIO $
                  atomicModifyIORef' mod_map_ref $ \mod_map ->
                    (M.insert ms_mod ir mod_map, ())
            }
        pure (h, liftIO $ readIORef mod_map_ref)
    void $ setSessionDynFlags dflags' {ghcMode = CompManager, hooks = h}
    traverse (`guessTarget` Nothing) targets >>= setTargets
    ok_flag <- load LoadAllTargets
    case ok_flag of
      Succeeded -> read_mod_map
      Failed -> liftIO $ throwGhcExceptionIO $ Panic "GHC.load returned Failed."

runCmm :: MonadIO m => Config -> [FilePath] -> m (M.Map FilePath CmmIR)
runCmm Config {..} cmm_fns =
  liftIO $
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
  runGhc (Just ghcLibDir) $ do
    dflags <- getSessionDynFlags
    (dflags', _, _) <- parseDynamicFlags dflags $ map noLoc ghcFlags
    (h, read_cmm_irs) <-
      liftIO $ do
        cmm_irs_ref <- newIORef []
        h <-
          hooksFromCompiler $
          defaultCompiler
            {withCmmIR = \ir -> liftIO $ modifyIORef' cmm_irs_ref (ir :)}
        pure (h, reverse <$> readIORef cmm_irs_ref)
    void $
      setSessionDynFlags
        dflags' {ghcMode = OneShot, ghcLink = NoLink, hooks = h}
    env <- getSession
    liftIO $ do
      oneShot env StopLn [(cmm_fn, Just CmmCpp) | cmm_fn <- cmm_fns]
      cmm_irs <- read_cmm_irs
      if length cmm_irs == length cmm_fns
        then pure $ M.fromList $ zip cmm_fns cmm_irs
        else throwGhcExceptionIO $ Panic "Unknown error when compiling .cmm"
