{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Toolkit.Hooks
  ( CompilerHooksOptions(..)
  , hooksFromCompiler
  ) where

import qualified CmmInfo as GHC
import Control.Concurrent
import Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified DriverPhases as GHC
import qualified DriverPipeline as GHC
import qualified DynFlags as GHC
import qualified Hooks as GHC
import qualified HscMain as GHC
import qualified HscTypes as GHC
import Language.Haskell.GHC.Toolkit.Compiler
import qualified PipelineMonad as GHC
import qualified StgCmm as GHC
import qualified Stream

newtype CompilerHooksOptions = CompilerHooksOptions
  { skipGCC :: Bool
  }

hooksFromCompiler :: Compiler -> CompilerHooksOptions -> IO GHC.Hooks
hooksFromCompiler Compiler {..} CompilerHooksOptions {..} = do
  mods_set_ref <- newMVar Set.empty
  parsed_map_ref <- newMVar Map.empty
  typechecked_map_ref <- newMVar Map.empty
  stg_map_ref <- newMVar Map.empty
  cmm_map_ref <- newMVar Map.empty
  cmm_raw_map_ref <- newMVar Map.empty
  cmm_ref <- newEmptyMVar
  cmm_raw_ref <- newEmptyMVar
  pure
    GHC.emptyHooks
      { GHC.tcRnModuleHook =
          Just $ \mod_summary@GHC.ModSummary {..} save_rn_syntax parsed_module' -> do
            parsed_module <- patchParsed mod_summary parsed_module'
            typechecked_module <-
              GHC.tcRnModule' mod_summary save_rn_syntax parsed_module >>=
              patchTypechecked mod_summary
            let store ref v =
                  modifyMVar_ ref $ \m -> pure $ Map.insert ms_mod v m
            liftIO $ do
              store parsed_map_ref parsed_module
              store typechecked_map_ref typechecked_module
            pure typechecked_module
      , GHC.stgCmmHook =
          Just $ \dflags this_mod data_tycons cost_centre_info stg_binds hpc_info -> do
            Stream.liftIO $
              modifyMVar_ stg_map_ref $ \stg_map ->
                pure $ Map.insert this_mod stg_binds stg_map
            GHC.codeGen
              dflags
              this_mod
              data_tycons
              cost_centre_info
              stg_binds
              hpc_info
      , GHC.cmmToRawCmmHook =
          Just $ \dflags maybe_ms_mod cmms -> do
            rawcmms <- GHC.cmmToRawCmm dflags maybe_ms_mod cmms
            case maybe_ms_mod of
              Just ms_mod -> do
                let store ref v =
                      modifyMVar_ ref $ \m -> pure $ Map.insert ms_mod v m
                store cmm_map_ref cmms
                store cmm_raw_map_ref rawcmms
              _ -> do
                putMVar cmm_ref cmms
                putMVar cmm_raw_ref rawcmms
            pure rawcmms
      , GHC.runPhaseHook =
          Just $ \phase input_fn dflags ->
            case phase of
              GHC.HscOut src_flavour _ (GHC.HscRecomp cgguts mod_summary@GHC.ModSummary {..}) -> do
                r <-
                  if skipGCC
                    then do
                      output_fn <-
                        GHC.phaseOutputFilename $
                        GHC.hscPostBackendPhase dflags src_flavour $
                        GHC.hscTarget dflags
                      GHC.PipeState {GHC.hsc_env = hsc_env'} <- GHC.getPipeState
                      (outputFilename, _, _) <-
                        liftIO $
                        GHC.hscGenHardCode hsc_env' cgguts mod_summary output_fn
                      GHC.setForeignOs []
                      pure (GHC.RealPhase GHC.StopLn, outputFilename)
                    else GHC.runPhase phase input_fn dflags
                f <-
                  liftIO $
                  modifyMVar mods_set_ref $ \s ->
                    pure (Set.insert ms_mod s, Set.member ms_mod s)
                if f
                  then liftIO $ do
                         let clean ref =
                               modifyMVar_ ref $ \m ->
                                 pure $ Map.delete ms_mod m
                         clean parsed_map_ref
                         clean typechecked_map_ref
                         clean stg_map_ref
                         clean cmm_map_ref
                         clean cmm_raw_map_ref
                  else (do let fetch ref =
                                 modifyMVar
                                   ref
                                   (\m ->
                                      let (Just v, m') =
                                            Map.updateLookupWithKey
                                              (\_ _ -> Nothing)
                                              ms_mod
                                              m
                                       in pure (m', v))
                           ir <-
                             liftIO $
                             HaskellIR <$> fetch parsed_map_ref <*>
                             fetch typechecked_map_ref <*>
                             pure cgguts <*>
                             fetch stg_map_ref <*>
                             (fetch cmm_map_ref >>= Stream.collect) <*>
                             (fetch cmm_raw_map_ref >>= Stream.collect)
                           withHaskellIR mod_summary ir)
                pure r
              GHC.RealPhase GHC.Cmm -> do
                r <-
                  if skipGCC
                    then do
                      output_fn <-
                        GHC.phaseOutputFilename $
                        GHC.hscPostBackendPhase dflags GHC.HsSrcFile $
                        GHC.hscTarget dflags
                      GHC.PipeState {hsc_env} <- GHC.getPipeState
                      liftIO $ GHC.hscCompileCmmFile hsc_env input_fn output_fn
                      pure (GHC.RealPhase GHC.StopLn, output_fn)
                    else GHC.runPhase phase input_fn dflags
                ir <-
                  liftIO $
                  CmmIR <$> (takeMVar cmm_ref >>= Stream.collect) <*>
                  (takeMVar cmm_raw_ref >>= Stream.collect)
                withCmmIR ir
                pure r
              _ -> GHC.runPhase phase input_fn dflags
      }
