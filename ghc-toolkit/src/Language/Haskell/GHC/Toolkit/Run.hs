{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}

module Language.Haskell.GHC.Toolkit.Run
  ( Config
  , ghcFlags
  , defaultConfig
  , run
  ) where

import CmdLineParser
import Control.Monad.IO.Class
import Data.Functor
import Data.IORef
import qualified Data.Map.Strict as M
import DynFlags
import GHC
import Language.Haskell.GHC.Toolkit.BuildInfo
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.Hooks
import Panic

newtype Config = Config
  { ghcFlags :: [String]
  }

defaultConfig :: Config
defaultConfig =
  Config
    { ghcFlags =
        [ "-Wall"
        , "-O2"
        , "-fforce-recomp"
        , "-no-keep-hi-files"
        , "-no-keep-o-files"
        ]
    }

run :: MonadIO m => Config -> [String] -> m (M.Map Module IR)
run Config {..} targets =
  liftIO $
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
  runGhc (Just ghcLibDir) $ do
    dflags <- getSessionDynFlags
    (dflags', lefts, warns) <- parseDynamicFlags dflags $ map noLoc ghcFlags
    case (# lefts, warns #) of
      (# [], [] #) -> do
        (h, read_mod_map) <-
          liftIO $ do
            mod_map_ref <- newIORef M.empty
            h <-
              hooksFromCompiler $
              defaultCompiler
                { withIR =
                    \ModSummary {..} ir ->
                      liftIO $
                      atomicModifyIORef' mod_map_ref $ \mod_map ->
                        (M.insert ms_mod ir mod_map, ())
                }
            pure (h, liftIO $ readIORef mod_map_ref)
        void $ setSessionDynFlags dflags' {ghcMode = CompManager, hooks = h}
        traverse (`guessTarget` Nothing) targets >>= setTargets
        ok_flag <- load LoadAllTargets
        case ok_flag of
          Succeeded -> read_mod_map
          Failed ->
            liftIO $ throwGhcExceptionIO $ Panic "GHC.load returned Failed."
      _ ->
        liftIO $
        throwGhcExceptionIO $
        Panic $
        "When parsing ghc options: " <> show ghcFlags <>
        ", leftover arguments: " <>
        show (map unLoc lefts) <>
        ", warnings: " <>
        show (map (unLoc . warnMsg) warns)
