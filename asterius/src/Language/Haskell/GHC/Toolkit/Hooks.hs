{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.GHC.Toolkit.Hooks
  ( hooksFromCompiler,
  )
where

import Control.Monad.IO.Class
import Distribution.Simple.Utils
import qualified DriverPhases as GHC
import qualified DriverPipeline as GHC
import qualified Hooks as GHC
import qualified HscMain as GHC
import qualified HscTypes as GHC
import Language.Haskell.GHC.Toolkit.Compiler
import qualified MkIface as GHC
import qualified PipelineMonad as GHC
import System.Directory
import System.FilePath

hooksFromCompiler :: Compiler -> GHC.Hooks -> IO GHC.Hooks
hooksFromCompiler Compiler {..} h = do
  tmpdir <- getTemporaryDirectory
  pure
    h
      { GHC.codeOutputHook = Just $ \dflags this_mod filenm _ _ _ _ cmm_stream ->
          do
            withCmmIR
              dflags
              this_mod
              (CmmIR cmm_stream)
              filenm
            pure (filenm, (False, Nothing), [], ()),
        GHC.runPhaseHook = Just $ \phase input_fn dflags -> case phase of
          GHC.HscOut _ _ GHC.HscRecomp {..} -> do
            output_fn <- GHC.phaseOutputFilename GHC.StopLn
            GHC.PipeState {..} <- GHC.getPipeState
            final_iface <-
              liftIO
                ( GHC.mkFullIface
                    hsc_env {GHC.hsc_dflags = hscs_iface_dflags}
                    hscs_partial_iface
                )
            _ <- GHC.P $
              \_env state -> pure (state {GHC.iface = Just final_iface}, ())
            liftIO $
              GHC.hscMaybeWriteIface
                hscs_iface_dflags
                final_iface
                hscs_old_iface_hash
                hscs_mod_location
            (_, _, _) <-
              liftIO $
                GHC.hscGenHardCode hsc_env hscs_guts hscs_mod_location output_fn
            GHC.setForeignOs []
            pure (GHC.RealPhase GHC.StopLn, output_fn)
          GHC.RealPhase GHC.CmmCpp -> do
            (_, next_input_fn') <- GHC.runPhase phase input_fn dflags
            next_input_fn <- liftIO $ do
              cmm_tmpdir <- createTempDirectory tmpdir "ahc"
              let next_input_fn = cmm_tmpdir </> takeFileName input_fn
              copyFile next_input_fn' next_input_fn
              pure next_input_fn
            pure (GHC.RealPhase GHC.Cmm, next_input_fn)
          GHC.RealPhase GHC.Cmm -> do
            output_fn <- GHC.phaseOutputFilename GHC.StopLn
            GHC.PipeState {hsc_env = hsc_env} <- GHC.getPipeState
            liftIO $ do
              GHC.hscCompileCmmFile hsc_env input_fn output_fn
              removePathForcibly $ takeDirectory input_fn
            pure (GHC.RealPhase GHC.StopLn, output_fn)
          _ -> GHC.runPhase phase input_fn dflags
      }
