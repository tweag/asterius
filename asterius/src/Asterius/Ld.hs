{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Ld
  ( LinkTask(..)
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

data LinkTask = LinkTask
  { linkOutput :: FilePath
  , linkObjs, linkLibs :: [FilePath]
  , debug, gcSections, binaryen :: Bool
  , outputIR :: Maybe FilePath
  , rootSymbols, exportFunctions :: [AsteriusEntitySymbol]
  } deriving (Show)

loadTheWorld :: BuiltinsOptions -> LinkTask -> IO AsteriusModule
loadTheWorld builtins_opts LinkTask {..} = do
  lib <- mconcat <$> for linkLibs loadAr
  objrs <- for linkObjs tryDecodeFile
  let objs = rights objrs
      builtins_mod = rtsAsteriusModule builtins_opts
  pure $ mconcat objs <> builtins_mod <> lib

rtsUsedSymbols :: Set AsteriusEntitySymbol
rtsUsedSymbols =
  Set.fromList
    [ "base_GHCziPtr_Ptr_con_info"
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
    , "stg_ARR_WORDS_info"
    , "stg_DEAD_WEAK_info"
    , "stg_NO_FINALIZER_closure"
    , "stg_WEAK_info"
    ]

linkExe :: LinkTask -> IO ()
linkExe ld_task@LinkTask {..} = do
  final_store <-
    loadTheWorld
      defaultBuiltinsOptions {Asterius.Builtins.debug = debug}
      ld_task
  let (pre_m, m, events, link_report) =
        linkStart
          debug
          True
          gcSections
          binaryen
          final_store
          (Set.unions
             [ Set.fromList rootSymbols
             , rtsUsedSymbols
             , Set.fromList
                 [ AsteriusEntitySymbol {entityName = internalName}
                 | FunctionExport {..} <- rtsFunctionExports debug True
                 ]
             ])
          exportFunctions
  encodeFile linkOutput (m, events, link_report)
  case outputIR of
    Just p -> writeFile p $ show pre_m
    _ -> pure ()
