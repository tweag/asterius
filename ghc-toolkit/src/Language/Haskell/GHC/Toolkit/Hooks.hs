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
import TcRnTypes

hooksFromCompiler :: MonadIO m => Compiler -> m Hooks
hooksFromCompiler c =
  liftIO $ do
    tc_map <- newIORef emptyModuleEnv
    pure $
      emptyHooks
        { hscFrontendHook =
            Just $ \mod_summary -> do
              let ms_mod0 = ms_mod mod_summary
              r@(FrontendTypecheck tc_env) <-
                genericHscFrontend'
                  (\mod_summary' p -> do
                     r <- patch c mod_summary' p
                     liftIO $
                       atomicModifyIORef' tc_map $ \m ->
                         ( extendModuleEnv
                             m
                             (ms_mod mod_summary')
                             (p, undefined)
                         , ())
                     pure r)
                  mod_summary
              liftIO $
                atomicModifyIORef' tc_map $ \m ->
                  let Just (p, _) = m `lookupModuleEnv` ms_mod0
                   in (extendModuleEnv m ms_mod0 (p, tc_env), ())
              pure r
        , runPhaseHook =
            Just $ \phase_plus input_fn dflags ->
              case phase_plus of
                HscOut src_flavour _ (HscRecomp cgguts mod_summary@ModSummary {..}) -> do
                  m_tc <-
                    liftIO $
                    atomicModifyIORef' tc_map $ \m ->
                      (m `delModuleEnv` ms_mod, m `lookupModuleEnv` ms_mod)
                  case m_tc of
                    Just (p, tc) -> do
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
                      withIR
                        c
                        mod_summary
                        IR
                          { parsed = p
                          , typeChecked = tc
                          , core = cgguts
                          , stg = _stg
                          , cmmRaw = _cmmRaw
                          }
                      pure (RealPhase next_phase, outputFilename)
                    _ -> runPhase phase_plus input_fn dflags
                _ -> runPhase phase_plus input_fn dflags
        }
