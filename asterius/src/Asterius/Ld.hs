{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Ld
  ( LinkTask (..),
    linkModules,
    linkExeInMemory,
    linkExe,
    rtsUsedSymbols,
  )
where

import Asterius.Ar
import Asterius.Binary.File
import Asterius.Binary.NameCache
import Asterius.Builtins
import Asterius.Builtins.Main
import Asterius.Resolve
import Asterius.Types
import qualified Asterius.Types.SymbolSet as SS
import Control.Exception
import Data.Either
import Data.Traversable

data LinkTask
  = LinkTask
      { progName, linkOutput :: FilePath,
        linkObjs, linkLibs :: [FilePath],
        linkModule :: AsteriusCachedModule,
        hasMain, debug, verboseErr :: Bool,
        rootSymbols, exportFunctions :: [EntitySymbol]
      }
  deriving (Show)

-- | Load all the library and object dependencies for a 'LinkTask' into a
-- single module. NOTE: object files in Haskell package directories can also
-- originate from gcc being called on cbits in packages. This in the past gave
-- deserialization failures. Hence, when we deserialize objects to be linked in
-- 'loadTheWorld', we choose to be overpermissive and silently ignore
-- deserialization failures. This has worked well so far.
loadTheWorld :: LinkTask -> IO AsteriusCachedModule
loadTheWorld LinkTask {..} = do
  ncu <- newNameCacheUpdater
  lib <- mconcat <$> for linkLibs (loadArchive ncu)
  objs <- rights <$> for linkObjs (tryGetFile ncu)
  evaluate $ linkModule <> mconcat objs <> lib

-- | The *_info are generated from Cmm using the INFO_TABLE macro.
-- For example, see StgMiscClosures.cmm / Exception.cmm
rtsUsedSymbols :: SS.SymbolSet
rtsUsedSymbols =
  SS.fromList
    [ "base_AsteriusziTopHandler_runIO_closure",
      "base_AsteriusziTopHandler_runNonIO_closure",
      "base_AsteriusziTypesziJSException_mkJSException_closure",
      "MainCapability",
      "stg_WHITEHOLE_info",
      "stg_IND_info",
      "stg_raise_info",
      "stg_raise_ret_info",
      "stg_JSVAL_info",
      "stg_STABLE_NAME_info"
    ]

rtsPrivateSymbols :: SS.SymbolSet
rtsPrivateSymbols =
  SS.fromList
    [ "base_AsteriusziTopHandler_runIO_closure",
      "base_AsteriusziTopHandler_runNonIO_closure"
    ]

linkModules ::
  LinkTask -> AsteriusCachedModule -> (Module, LinkReport)
linkModules LinkTask {..} m =
  linkStart
    debug
    ( toCachedModule
        ( (if hasMain then mainBuiltins else mempty)
            <> rtsAsteriusModule
              defaultBuiltinsOptions
                { Asterius.Builtins.progName = progName,
                  Asterius.Builtins.debug = debug
                }
        )
        <> m
    )
    ( SS.unions
        [ SS.fromList rootSymbols,
          rtsUsedSymbols,
          rtsPrivateSymbols,
          SS.fromList
            [ mkEntitySymbol internalName
              | FunctionExport {..} <- rtsFunctionExports
            ]
        ]
    )
    exportFunctions

linkExeInMemory :: LinkTask -> IO (Module, LinkReport)
linkExeInMemory ld_task = do
  final_store <- loadTheWorld ld_task
  evaluate $ linkModules ld_task final_store

linkExe :: LinkTask -> IO ()
linkExe ld_task@LinkTask {..} = do
  (m, link_report) <- linkExeInMemory ld_task
  putFile linkOutput (m, link_report)
