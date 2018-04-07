{-# LANGUAGE RecordWildCards #-}

module Asterius.FrontendPlugin
  ( frontendPlugin
  ) where

import Asterius.CodeGen
import GhcPlugins
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.FrontendPlugin
import Language.Haskell.GHC.Toolkit.ObjectStore
import System.Environment
import Text.Show.Pretty

frontendPlugin :: FrontendPlugin
frontendPlugin =
  frontendPluginFromCompiler $ do
    Store {..} <-
      liftIO $ do
        obj_topdir <- getEnv "ASTERIUS_LIB_DIR"
        newStore
          StoreConfig
            { storeTopDir = obj_topdir
            , objectExt = "txt"
            , cacheObject = True
            , rawRead = readFile
            , rawWrite = writeFile
            }
    pure $
      defaultCompiler
        { withIR =
            \ModSummary {..} ir -> do
              dflags <- getDynFlags
              liftIO $ do
                m <- marshalIR dflags ir
                objectWrite ms_mod $ ppShow m
        }
