{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.GHC.Toolkit.Hooks
  ( hooksFromCompiler,
  )
where

import qualified CmmInfo as GHC
import Control.Concurrent
import Control.Monad.IO.Class
import Data.Functor
import qualified DriverPhases as GHC
import qualified DriverPipeline as GHC
import qualified DynFlags as GHC
import qualified Hooks as GHC
import qualified HscMain as GHC
import qualified HscTypes as GHC
import Language.Haskell.GHC.Toolkit.Compiler
import qualified Module as GHC
import qualified PipelineMonad as GHC

hooksFromCompiler :: Compiler -> GHC.Hooks -> IO GHC.Hooks
hooksFromCompiler Compiler {..} h = do
  cmm_raw_map_ref <- newMVar GHC.emptyModuleEnv
  cmm_raw_ref <- newEmptyMVar
  pure
    h
      { GHC.cmmToRawCmmHook = Just $ \dflags maybe_ms_mod cmms -> do
          rawcmms <- GHC.cmmToRawCmm dflags maybe_ms_mod cmms
          case maybe_ms_mod of
            Just ms_mod -> do
              let store :: MVar (GHC.ModuleEnv v) -> v -> IO ()
                  store ref v =
                    modifyMVar_ ref $ \env -> pure $ GHC.extendModuleEnv env ms_mod v
              store cmm_raw_map_ref rawcmms
            _ -> putMVar cmm_raw_ref rawcmms
          pure rawcmms,
        GHC.runPhaseHook = Just $ \phase input_fn dflags -> case phase of
          GHC.HscOut src_flavour _ (GHC.HscRecomp cgguts mod_summary@GHC.ModSummary {..}) ->
            do
              r@(_, obj_output_fn) <- do
                output_fn <-
                  GHC.phaseOutputFilename
                    $ GHC.hscPostBackendPhase dflags src_flavour
                    $ GHC.hscTarget dflags
                GHC.PipeState {GHC.hsc_env = hsc_env'} <- GHC.getPipeState
                void $ liftIO $
                  GHC.hscGenHardCode
                    hsc_env'
                    cgguts
                    mod_summary
                    output_fn
                GHC.setForeignOs []
                obj_output_fn <- GHC.phaseOutputFilename GHC.StopLn
                pure (GHC.RealPhase GHC.StopLn, obj_output_fn)
              let fetch :: MVar (GHC.ModuleEnv v) -> IO v
                  fetch ref =
                    modifyMVar
                      ref
                      ( \env ->
                          let Just v = GHC.lookupModuleEnv env ms_mod
                           in pure (GHC.delModuleEnv env ms_mod, v)
                      )
              ir <- liftIO $ HaskellIR <$> fetch cmm_raw_map_ref
              withHaskellIR mod_summary ir obj_output_fn
              pure r
          GHC.RealPhase GHC.Cmm -> do
            void $ GHC.runPhase phase input_fn dflags
            ir <- liftIO $ CmmIR <$> takeMVar cmm_raw_ref
            obj_output_fn <- GHC.phaseOutputFilename GHC.StopLn
            withCmmIR ir obj_output_fn
            pure (GHC.RealPhase GHC.StopLn, obj_output_fn)
          _ -> GHC.runPhase phase input_fn dflags
      }
