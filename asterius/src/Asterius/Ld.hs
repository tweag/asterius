{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Ld
  ( LinkTask(..)
  , linkModules
  , linkExe
  , rtsUsedSymbols
  ) where

import Asterius.Ar
import Asterius.Builtins
import Asterius.Internals
import Asterius.Resolve
import Asterius.Types
import Data.Either
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Traversable
import Prelude hiding (IO)
import System.FilePath

data LinkTask = LinkTask
  { linkOutput :: FilePath
  , linkObjs, linkLibs :: [FilePath]
  , debug, gcSections, binaryen, verboseErr :: Bool
  , outputIR :: Maybe FilePath
  , rootSymbols, exportFunctions :: [AsteriusEntitySymbol]
  } deriving (Show)

loadTheWorld :: LinkTask -> IO AsteriusModule
loadTheWorld LinkTask {..} = do
  lib <- mconcat <$> for linkLibs loadAr
  objrs <- for linkObjs tryDecodeFile
  let objs = rights objrs
  pure $ mconcat objs <> lib

-- | The *_info are generated from Cmm using the INFO_TABLE macro.
-- For example, see StgMiscClosures.cmm / Exception.cmm
rtsUsedSymbols :: Set AsteriusEntitySymbol
rtsUsedSymbols =
  Set.fromList
    [ "__asterius_func"
    , "__asterius_regs"
    , "__asterius_ret"
    , "barf"
    , "base_GHCziPtr_Ptr_con_info"
    , "base_GHCziStable_StablePtr_con_info"
    , "ghczmprim_GHCziTypes_Czh_con_info"
    , "ghczmprim_GHCziTypes_Dzh_con_info"
    , "ghczmprim_GHCziTypes_False_closure"
    , "ghczmprim_GHCziTypes_Izh_con_info"
    , "ghczmprim_GHCziTypes_True_closure"
    , "ghczmprim_GHCziTypes_Wzh_con_info"
    , "ghczmprim_GHCziTypes_ZC_con_info"
    , "ghczmprim_GHCziTypes_ZMZN_closure"
    , "integerzmwiredzmin_GHCziIntegerziType_Integer_con_info"
    , "MainCapability"
    , "Main_main_closure"
    , "stg_ARR_WORDS_info"
    , "stg_BLACKHOLE_info"
    , "stg_DEAD_WEAK_info"
    , "stg_marked_upd_frame_info"
    , "stg_NO_FINALIZER_closure"
    , "stg_raise_info"
    , "stg_returnToStackTop"
    , "stg_STABLE_NAME_info"
    , "stg_WEAK_info"
    ]

linkModules ::
     LinkTask -> AsteriusModule -> (AsteriusModule, Module, LinkReport)
linkModules LinkTask {..} m =
  linkStart
    debug
    gcSections
    binaryen
    verboseErr
    (rtsAsteriusModule
       defaultBuiltinsOptions
         {progName = takeBaseName linkOutput, Asterius.Builtins.debug = debug} <>
     m)
    (Set.unions
       [ Set.fromList rootSymbols
       , rtsUsedSymbols
       , Set.fromList
           [ AsteriusEntitySymbol {entityName = internalName}
           | FunctionExport {..} <- rtsFunctionExports debug
           ]
       ])
    exportFunctions

linkExeInMemory :: LinkTask -> IO (AsteriusModule, Module, LinkReport)
linkExeInMemory ld_task = do
  final_store <- loadTheWorld ld_task
  pure $ linkModules ld_task final_store

linkExe :: LinkTask -> IO ()
linkExe ld_task@LinkTask {..} = do
  (pre_m, m, link_report) <- linkExeInMemory ld_task
  encodeFile linkOutput (m, link_report)
  case outputIR of
    Just p -> encodeFile p pre_m
    _ -> pure ()
