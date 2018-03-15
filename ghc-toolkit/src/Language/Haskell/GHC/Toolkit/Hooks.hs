{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.GHC.Toolkit.Hooks
  ( hooksFromCompiler
  ) where

import Control.Monad.IO.Class
import Data.IORef
import DriverPipeline
import DynFlags
import Hooks
import HscTypes
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.GHCUnexported
import Module
import PipelineMonad

hooksFromCompiler :: Compiler -> IO Hooks
hooksFromCompiler c = do
  first_run_modules <- newIORef emptyModuleSet
  pure
    emptyHooks
      { runPhaseHook =
          Just $ \phase_plus input_fn dflags ->
            case phase_plus of
              HscOut src_flavour _ (HscRecomp cgguts mod_summary@ModSummary {..}) -> do
                first_run <-
                  liftIO $
                  atomicModifyIORef' first_run_modules $ \s ->
                    if elemModuleSet ms_mod s
                      then (delModuleSet s ms_mod, False)
                      else (extendModuleSet s ms_mod, True)
                if first_run
                  then do
                    let hsc_lang = hscTarget dflags
                        next_phase =
                          hscPostBackendPhase dflags src_flavour hsc_lang
                    output_fn <- phaseOutputFilename next_phase
                    PipeState {hsc_env = hsc_env'} <- getPipeState
                    (outputFilename, mStub, foreign_files, _stg, _cmmRaw) <-
                      liftIO $
                      hscGenHardCode' hsc_env' cgguts mod_summary output_fn
                    stub_o <- liftIO (mapM (compileStub hsc_env') mStub)
                    foreign_os <-
                      liftIO $
                      mapM (uncurry (compileForeign hsc_env')) foreign_files
                    setForeignOs (maybe [] return stub_o ++ foreign_os)
                    liftIO $
                      withIR
                        c
                        mod_summary
                        IR {core = cgguts, stg = _stg, cmmRaw = _cmmRaw}
                    pure (RealPhase next_phase, outputFilename)
                  else runPhase phase_plus input_fn dflags
              _ -> runPhase phase_plus input_fn dflags
      }
