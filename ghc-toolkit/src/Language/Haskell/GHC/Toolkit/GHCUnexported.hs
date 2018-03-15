{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.Haskell.GHC.Toolkit.GHCUnexported where

import Cmm
import CmmBuildInfoTables
import CmmInfo
import CmmPipeline
import CodeOutput
import Control.Monad
import Control.Monad.IO.Class
import CorePrep
import CoreSyn
import CoreToStg
import CostCentre
import Data.Maybe
import qualified Data.Set as S
import DriverPhases
import DriverPipeline
import DynFlags
import ErrUtils
import FileCleanup
import Hooks
import HscMain
import HscTypes
import Module
import Outputable
import Panic
import PipelineMonad
import Platform
import ProfInit
import SimplStg
import StgCmm
import StgSyn
import qualified Stream
import Stream (Stream)
import SysTools
import System.FilePath
import TyCon
import UniqSupply

compileForeign :: HscEnv -> ForeignSrcLang -> FilePath -> IO FilePath
compileForeign hsc_env lang stub_c = do
        let phase = case lang of
              LangC -> Cc
              LangCxx -> Ccxx
              LangObjc -> Cobjc
              LangObjcxx -> Cobjcxx
        (_, stub_o) <- runPipeline StopLn hsc_env
                       (stub_c, Just (RealPhase phase))
                       Nothing (Temporary TFL_GhcSession)
                       Nothing{-no ModLocation-}
                       []
        return stub_o

compileStub :: HscEnv -> FilePath -> IO FilePath
compileStub hsc_env stub_c = compileForeign hsc_env LangC stub_c

runPipeline
  :: Phase                      -- ^ When to stop
  -> HscEnv                     -- ^ Compilation environment
  -> (FilePath,Maybe PhasePlus) -- ^ Input filename (and maybe -x suffix)
  -> Maybe FilePath             -- ^ original basename (if different from ^^^)
  -> PipelineOutput             -- ^ Output filename
  -> Maybe ModLocation          -- ^ A ModLocation, if this is a Haskell module
  -> [FilePath]                 -- ^ foreign objects
  -> IO (DynFlags, FilePath)    -- ^ (final flags, output filename)
runPipeline stop_phase hsc_env0 (input_fn, mb_phase)
             mb_basename output maybe_loc foreign_os

    = do let
             dflags0 = hsc_dflags hsc_env0

             -- Decide where dump files should go based on the pipeline output
             dflags = dflags0 { dumpPrefix = Just (basename ++ ".") }
             hsc_env = hsc_env0 {hsc_dflags = dflags}

             (input_basename, suffix) = splitExtension input_fn
             suffix' = drop 1 suffix -- strip off the .
             basename | Just b <- mb_basename = b
                      | otherwise             = input_basename

             -- If we were given a -x flag, then use that phase to start from
             start_phase = fromMaybe (RealPhase (startPhase suffix')) mb_phase

             isHaskell (RealPhase (Unlit _)) = True
             isHaskell (RealPhase (Cpp   _)) = True
             isHaskell (RealPhase (HsPp  _)) = True
             isHaskell (RealPhase (DriverPhases.Hsc   _)) = True
             isHaskell (HscOut {})           = True
             isHaskell _                     = False

             isHaskellishFile = isHaskell start_phase

             env = PipeEnv{ stop_phase,
                            src_filename = input_fn,
                            src_basename = basename,
                            src_suffix = suffix',
                            output_spec = output }

         when (isBackpackishSuffix suffix') $
           throwGhcExceptionIO (UsageError
                       ("use --backpack to process " ++ input_fn))

         -- We want to catch cases of "you can't get there from here" before
         -- we start the pipeline, because otherwise it will just run off the
         -- end.
         let happensBefore' = happensBefore dflags
         case start_phase of
             RealPhase start_phase' ->
                 -- See Note [Partial ordering on phases]
                 -- Not the same as: (stop_phase `happensBefore` start_phase')
                 when (not (start_phase' `happensBefore'` stop_phase ||
                            start_phase' `eqPhase` stop_phase)) $
                       throwGhcExceptionIO (UsageError
                                   ("cannot compile this file to desired target: "
                                      ++ input_fn))
             HscOut {} -> return ()

         debugTraceMsg dflags 4 (text "Running the pipeline")
         r <- runPipeline' start_phase hsc_env env input_fn
                           maybe_loc foreign_os

         -- If we are compiling a Haskell module, and doing
         -- -dynamic-too, but couldn't do the -dynamic-too fast
         -- path, then rerun the pipeline for the dyn way
         let dflags = hsc_dflags hsc_env
         -- NB: Currently disabled on Windows (ref #7134, #8228, and #5987)
         when (not $ platformOS (targetPlatform dflags) == OSMinGW32) $ do
           when isHaskellishFile $ whenCannotGenerateDynamicToo dflags $ do
               debugTraceMsg dflags 4
                   (text "Running the pipeline again for -dynamic-too")
               let dflags' = dynamicTooMkDynamicDynFlags dflags
               hsc_env' <- newHscEnv dflags'
               _ <- runPipeline' start_phase hsc_env' env input_fn
                                 maybe_loc foreign_os
               return ()
         return r

runPipeline'
  :: PhasePlus                  -- ^ When to start
  -> HscEnv                     -- ^ Compilation environment
  -> PipeEnv
  -> FilePath                   -- ^ Input filename
  -> Maybe ModLocation          -- ^ A ModLocation, if this is a Haskell module
  -> [FilePath]                 -- ^ foreign objects, if we have one
  -> IO (DynFlags, FilePath)    -- ^ (final flags, output filename)
runPipeline' start_phase hsc_env env input_fn
             maybe_loc foreign_os
  = do
  -- Execute the pipeline...
  let state = PipeState{ hsc_env, maybe_loc, foreign_os = foreign_os }

  evalP (pipeLoop start_phase input_fn) env state

pipeLoop :: PhasePlus -> FilePath -> CompPipeline (DynFlags, FilePath)
pipeLoop phase input_fn = do
  env <- getPipeEnv
  dflags <- getDynFlags
  -- See Note [Partial ordering on phases]
  let happensBefore' = happensBefore dflags
      stopPhase = stop_phase env
  case phase of
   RealPhase realPhase | realPhase `eqPhase` stopPhase            -- All done
     -> -- Sometimes, a compilation phase doesn't actually generate any output
        -- (eg. the CPP phase when -fcpp is not turned on).  If we end on this
        -- stage, but we wanted to keep the output, then we have to explicitly
        -- copy the file, remembering to prepend a {-# LINE #-} pragma so that
        -- further compilation stages can tell what the original filename was.
        case output_spec env of
        Temporary _ ->
            return (dflags, input_fn)
        output ->
            do pst <- getPipeState
               final_fn <- liftIO $ getOutputFilename
                                        stopPhase output (src_basename env)
                                        dflags stopPhase (maybe_loc pst)
               when (final_fn /= input_fn) $ do
                  let msg = ("Copying `" ++ input_fn ++"' to `" ++ final_fn ++ "'")
                      line_prag = Just ("{-# LINE 1 \"" ++ src_filename env ++ "\" #-}\n")
                  liftIO $ copyWithHeader dflags msg line_prag input_fn final_fn
               return (dflags, final_fn)


     | not (realPhase `happensBefore'` stopPhase)
        -- Something has gone wrong.  We'll try to cover all the cases when
        -- this could happen, so if we reach here it is a panic.
        -- eg. it might happen if the -C flag is used on a source file that
        -- has {-# OPTIONS -fasm #-}.
     -> panic ("pipeLoop: at phase " ++ show realPhase ++
           " but I wanted to stop at phase " ++ show stopPhase)

   _
     -> do liftIO $ debugTraceMsg dflags 4
                                  (text "Running phase" <+> ppr phase)
           (next_phase, output_fn) <- runHookedPhase phase input_fn dflags
           r <- pipeLoop next_phase output_fn
           case phase of
               HscOut {} ->
                   whenGeneratingDynamicToo dflags $ do
                       setDynFlags $ dynamicTooMkDynamicDynFlags dflags
                       -- TODO shouldn't ignore result:
                       _ <- pipeLoop phase input_fn
                       return ()
               _ ->
                   return ()
           return r

runHookedPhase :: PhasePlus -> FilePath -> DynFlags
               -> CompPipeline (PhasePlus, FilePath)
runHookedPhase pp input dflags =
  lookupHook runPhaseHook runPhase dflags pp input dflags

hscGenHardCode' :: HscEnv -> CgGuts -> ModSummary -> FilePath
               -> IO (FilePath, Maybe FilePath, [(ForeignSrcLang, FilePath)], [StgTopBinding], [RawCmmDecl])
               -- ^ @Just f@ <=> _stub.c is f
hscGenHardCode' hsc_env cgguts mod_summary output_filename = do
        let CgGuts{ -- This is the last use of the ModGuts in a compilation.
                    -- From now on, we just use the bits we need.
                    cg_module   = this_mod,
                    cg_binds    = core_binds,
                    cg_tycons   = tycons,
                    cg_foreign  = foreign_stubs0,
                    cg_foreign_files = foreign_files,
                    cg_dep_pkgs = dependencies,
                    cg_hpc_info = hpc_info } = cgguts
            dflags = hsc_dflags hsc_env
            location = ms_location mod_summary
            data_tycons = filter isDataTyCon tycons
            -- cg_tycons includes newtypes, for the benefit of External Core,
            -- but we don't generate any code for newtypes

        -------------------
        -- PREPARE FOR CODE GENERATION
        -- Do saturation and convert to A-normal form
        (prepd_binds, local_ccs) <- {-# SCC "CorePrep" #-}
                       corePrepPgm hsc_env this_mod location
                                   core_binds data_tycons
        -----------------  Convert to STG ------------------
        (stg_binds, (caf_ccs, caf_cc_stacks))
            <- {-# SCC "CoreToStg" #-}
               myCoreToStg dflags this_mod prepd_binds

        let cost_centre_info =
              (S.toList local_ccs ++ caf_ccs, caf_cc_stacks)
            prof_init = profilingInitCode this_mod cost_centre_info
            foreign_stubs = foreign_stubs0 `appendStubC` prof_init

        ------------------  Code generation ------------------

        -- The back-end is streamed: each top-level function goes
        -- from Stg all the way to asm before dealing with the next
        -- top-level function, so showPass isn't very useful here.
        -- Hence we have one showPass for the whole backend, the
        -- next showPass after this will be "Assembler".
        withTiming (pure dflags)
                   (text "CodeGen"<+>brackets (ppr this_mod))
                   (const ()) $ do
            cmms <- {-# SCC "StgCmm" #-}
                            doCodeGen hsc_env this_mod data_tycons
                                cost_centre_info
                                stg_binds hpc_info

            ------------------  Code output -----------------------
            rawcmms0 <- {-# SCC "cmmToRawCmm" #-}
                      cmmToRawCmm dflags cmms

            rawcmms_list <- concat <$> Stream.collect rawcmms0

            let dump a = do dumpIfSet_dyn dflags Opt_D_dump_cmm_raw "Raw Cmm"
                              (ppr a)
                            return a
                rawcmms1 = Stream.mapM dump rawcmms0

            (output_filename, (_stub_h_exists, stub_c_exists), foreign_fps)
                <- {-# SCC "codeOutput" #-}
                  codeOutput dflags this_mod output_filename location
                  foreign_stubs foreign_files dependencies rawcmms1
            return (output_filename, stub_c_exists, foreign_fps, stg_binds, rawcmms_list)

doCodeGen   :: HscEnv -> Module -> [TyCon]
            -> CollectedCCs
            -> [StgTopBinding]
            -> HpcInfo
            -> IO (Stream IO CmmGroup ())
         -- Note we produce a 'Stream' of CmmGroups, so that the
         -- backend can be run incrementally.  Otherwise it generates all
         -- the C-- up front, which has a significant space cost.
doCodeGen hsc_env this_mod data_tycons
              cost_centre_info stg_binds hpc_info = do
    let dflags = hsc_dflags hsc_env

    let cmm_stream :: Stream IO CmmGroup ()
        cmm_stream = {-# SCC "StgCmm" #-}
            StgCmm.codeGen dflags this_mod data_tycons
                           cost_centre_info stg_binds hpc_info

        -- codegen consumes a stream of CmmGroup, and produces a new
        -- stream of CmmGroup (not necessarily synchronised: one
        -- CmmGroup on input may produce many CmmGroups on output due
        -- to proc-point splitting).

    let dump1 a = do dumpIfSet_dyn dflags Opt_D_dump_cmm_from_stg
                       "Cmm produced by codegen" (ppr a)
                     return a

        ppr_stream1 = Stream.mapM dump1 cmm_stream

    -- We are building a single SRT for the entire module, so
    -- we must thread it through all the procedures as we cps-convert them.
    us <- mkSplitUniqSupply 'S'

    -- When splitting, we generate one SRT per split chunk, otherwise
    -- we generate one SRT for the whole module.
    let
     pipeline_stream
      | gopt Opt_SplitObjs dflags || gopt Opt_SplitSections dflags ||
        osSubsectionsViaSymbols (platformOS (targetPlatform dflags))
        = {-# SCC "cmmPipeline" #-}
          let run_pipeline us cmmgroup = do
                let (topSRT', us') = initUs us emptySRT
                (topSRT, cmmgroup) <- cmmPipeline hsc_env topSRT' cmmgroup
                let srt | isEmptySRT topSRT = []
                        | otherwise         = srtToData topSRT
                return (us', srt ++ cmmgroup)

          in do _ <- Stream.mapAccumL run_pipeline us ppr_stream1
                return ()

      | otherwise
        = {-# SCC "cmmPipeline" #-}
          let initTopSRT = initUs_ us emptySRT
              run_pipeline = cmmPipeline hsc_env
          in do topSRT <- Stream.mapAccumL run_pipeline initTopSRT ppr_stream1
                Stream.yield (srtToData topSRT)

    let
        dump2 a = do dumpIfSet_dyn dflags Opt_D_dump_cmm
                        "Output Cmm" (ppr a)
                     return a

        ppr_stream2 = Stream.mapM dump2 pipeline_stream

    return ppr_stream2

myCoreToStg :: DynFlags -> Module -> CoreProgram
            -> IO ( [StgTopBinding] -- output program
                  , CollectedCCs )  -- CAF cost centre info (declared and used)
myCoreToStg dflags this_mod prepd_binds = do
    let (stg_binds, cost_centre_info)
         = {-# SCC "Core2Stg" #-}
           coreToStg dflags this_mod prepd_binds

    stg_binds2
        <- {-# SCC "Stg2Stg" #-}
           stg2stg dflags stg_binds

    return (stg_binds2, cost_centre_info)
