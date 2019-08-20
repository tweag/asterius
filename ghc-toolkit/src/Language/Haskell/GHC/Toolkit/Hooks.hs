{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.GHC.Toolkit.Hooks
  ( hooksFromCompiler
    )
where

import qualified CmmInfo as GHC
import Control.Concurrent
import Control.Monad.IO.Class
import Data.Functor
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified DriverPhases as GHC
import qualified DriverPipeline as GHC
import qualified DynFlags as GHC
import qualified Hooks as GHC
import qualified HscMain as GHC
import qualified HscTypes as GHC
import Language.Haskell.GHC.Toolkit.Compiler
import qualified Module as GHC
import qualified PipelineMonad as GHC
import qualified StgCmm as GHC
import qualified Stream

hooksFromCompiler :: Compiler -> GHC.Hooks -> IO GHC.Hooks
hooksFromCompiler Compiler {..} h = do
  mods_set_ref <- newMVar Set.empty
  stg_map_ref <- newMVar Map.empty
  cmm_raw_map_ref <- newMVar Map.empty
  cmm_raw_ref <- newEmptyMVar
  pure
    h
      { GHC.stgCmmHook = Just
          $ \dflags this_mod data_tycons cost_centre_info stg_binds hpc_info -> do
            Stream.liftIO
              $ modifyMVar_ stg_map_ref
              $ pure
              . Map.insert this_mod stg_binds
            GHC.codeGen dflags
              this_mod
              data_tycons
              cost_centre_info
              stg_binds
              hpc_info,
        GHC.cmmToRawCmmHook = Just $ \dflags maybe_ms_mod cmms -> do
          rawcmms <- GHC.cmmToRawCmm dflags maybe_ms_mod cmms
          case maybe_ms_mod of
            Just ms_mod -> do
              let store :: MVar (Map.Map GHC.Module v) -> v -> IO ()
                  store ref v = modifyMVar_ ref $ pure . Map.insert ms_mod v
              store cmm_raw_map_ref rawcmms
            _ -> putMVar cmm_raw_ref rawcmms
          pure rawcmms,
        GHC.runPhaseHook = Just $ \phase input_fn dflags -> case phase of
          GHC.HscOut src_flavour _ (GHC.HscRecomp cgguts mod_summary@GHC.ModSummary {..}) ->
            do
              r@(_, obj_output_fn) <-
                do
                  output_fn <-
                    GHC.phaseOutputFilename
                      $ GHC.hscPostBackendPhase dflags src_flavour
                      $ GHC.hscTarget dflags
                  GHC.PipeState {GHC.hsc_env = hsc_env'} <- GHC.getPipeState
                  void $ liftIO
                    $ GHC.hscGenHardCode hsc_env'
                        cgguts
                        mod_summary
                        output_fn
                  GHC.setForeignOs []
                  obj_output_fn <- GHC.phaseOutputFilename GHC.StopLn
                  pure (GHC.RealPhase GHC.StopLn, obj_output_fn)
              f <-
                liftIO $ modifyMVar mods_set_ref $ \s ->
                  pure (Set.insert ms_mod s, Set.member ms_mod s)
              if f
              then
                liftIO $ do
                  let clean :: MVar (Map.Map GHC.Module a) -> IO ()
                      clean ref = modifyMVar_ ref $ pure . Map.delete ms_mod
                  clean stg_map_ref
                  clean cmm_raw_map_ref
              else
                ( do
                    let fetch :: MVar (Map.Map GHC.Module v) -> IO v
                        fetch ref =
                          modifyMVar
                            ref
                            ( \m ->
                                let (Just v, m') =
                                      Map.updateLookupWithKey
                                        (\_ _ -> Nothing)
                                        ms_mod
                                        m
                                 in pure (m', v)
                              )
                    ir <-
                      liftIO
                        $ HaskellIR
                        <$> fetch stg_map_ref
                        <*> (fetch cmm_raw_map_ref >>= Stream.collect)
                    withHaskellIR mod_summary ir obj_output_fn
                  )
              pure r
          GHC.RealPhase GHC.Cmm -> do
            void $ GHC.runPhase phase input_fn dflags
            ir <- liftIO $ CmmIR <$> (takeMVar cmm_raw_ref >>= Stream.collect)
            obj_output_fn <- GHC.phaseOutputFilename GHC.StopLn
            withCmmIR ir obj_output_fn
            pure (GHC.RealPhase GHC.StopLn, obj_output_fn)
          _ -> GHC.runPhase phase input_fn dflags
        }
