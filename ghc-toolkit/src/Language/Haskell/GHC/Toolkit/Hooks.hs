{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.GHC.Toolkit.Hooks
  ( hooksFromCompiler,
  )
where

import Control.Monad.IO.Class
import qualified DriverPhases as GHC
import qualified DriverPipeline as GHC
import qualified Hooks as GHC
import qualified HscMain as GHC
import qualified HscTypes as GHC
import Language.Haskell.GHC.Toolkit.Compiler
import qualified PipelineMonad as GHC

hooksFromCompiler :: Compiler -> GHC.Hooks -> IO GHC.Hooks
hooksFromCompiler Compiler {..} h =
  pure
    h
      { GHC.codeOutputHook = Just $ \dflags this_mod filenm _ _ _ _ cmm_stream -> do
          withCmmIR dflags this_mod (CmmIR cmm_stream) filenm
          pure (filenm, (False, Nothing), []),
        GHC.runPhaseHook = Just $ \phase input_fn dflags -> case phase of
          GHC.HscOut _ _ (GHC.HscRecomp cgguts mod_summary) -> do
            output_fn <- GHC.phaseOutputFilename GHC.StopLn
            liftIO $
              withHaskellIR
                dflags
                (GHC.ms_mod mod_summary)
                (HaskellIR cgguts)
                output_fn
            GHC.PipeState {hsc_env = hsc_env} <- GHC.getPipeState
            (_, _, _) <-
              liftIO $
                GHC.hscGenHardCode hsc_env cgguts mod_summary output_fn
            GHC.setForeignOs []
            pure (GHC.RealPhase GHC.StopLn, output_fn)
          GHC.RealPhase GHC.Cmm -> do
            output_fn <- GHC.phaseOutputFilename GHC.StopLn
            GHC.PipeState {hsc_env = hsc_env} <- GHC.getPipeState
            liftIO $ GHC.hscCompileCmmFile hsc_env input_fn output_fn
            pure (GHC.RealPhase GHC.StopLn, output_fn)
          _ -> GHC.runPhase phase input_fn dflags
      }
