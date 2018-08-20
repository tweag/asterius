{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.GHC.Toolkit.Hooks
  ( hooksFromCompiler
  ) where

import Control.Monad.IO.Class
import Data.IORef
import Data.Maybe
import DriverPhases
import DriverPipeline
import DynFlags
import Hooks
import HscMain
import HscTypes
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.GHCUnexported
import Module
import PipelineMonad
import System.Environment
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
              FrontendTypecheck tc_env0 <-
                genericHscFrontend'
                  (\mod_summary' p -> do
                     r <- patchParsed c mod_summary' p
                     liftIO $
                       atomicModifyIORef' tc_map $ \m ->
                         ( extendModuleEnv
                             m
                             (ms_mod mod_summary')
                             (r, undefined)
                         , ())
                     pure r)
                  mod_summary
              tc_env <- patchTypechecked c mod_summary tc_env0
              liftIO $
                atomicModifyIORef' tc_map $ \m ->
                  let Just (p, _) = m `lookupModuleEnv` ms_mod0
                   in (extendModuleEnv m ms_mod0 (p, tc_env), ())
              pure $ FrontendTypecheck tc_env
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
                      (outputFilename, mStub, foreign_files, _stg, _cmm, _cmmRaw) <-
                        liftIO $
                        hscGenHardCode' hsc_env' cgguts mod_summary output_fn
                      skip_gcc <-
                        isJust <$> liftIO (lookupEnv "GHC_TOOLKIT_SKIP_GCC")
                      if skip_gcc
                        then setForeignOs []
                        else do
                          stub_o <- liftIO (mapM (compileStub hsc_env') mStub)
                          foreign_os <-
                            liftIO $
                            mapM
                              (uncurry (compileForeign hsc_env'))
                              foreign_files
                          setForeignOs (maybe [] return stub_o ++ foreign_os)
                      withHaskellIR
                        c
                        mod_summary
                        HaskellIR
                          { parsed = p
                          , typechecked = tc
                          , core = cgguts
                          , stg = _stg
                          , cmm = _cmm
                          , cmmRaw = _cmmRaw
                          }
                      pure
                        ( RealPhase $
                          if skip_gcc
                            then StopLn
                            else next_phase
                        , outputFilename)
                    _ -> runPhase phase_plus input_fn dflags
                RealPhase Cmm -> do
                  let hsc_lang = hscTarget dflags
                  let next_phase = hscPostBackendPhase dflags HsSrcFile hsc_lang
                  output_fn <- phaseOutputFilename next_phase
                  PipeState {hsc_env} <- getPipeState
                  (cs, rcs) <-
                    liftIO $ hscCompileCmmFile' hsc_env input_fn output_fn
                  withCmmIR c CmmIR {cmm = cs, cmmRaw = rcs}
                  skip_gcc <-
                    isJust <$> liftIO (lookupEnv "GHC_TOOLKIT_SKIP_GCC")
                  pure
                    ( RealPhase $
                      if skip_gcc
                        then StopLn
                        else next_phase
                    , output_fn)
                _ -> runPhase phase_plus input_fn dflags
        }
