{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.FrontendPlugin
  ( frontendPlugin
  ) where

import GHC
import GhcPlugins
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.FrontendPlugin
import Language.Haskell.GHC.Toolkit.ObjectStore
import Language.Haskell.GHC.Toolkit.Orphans.Show ()
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
            , objectExt = "ddump-cmm-raw-ast"
            , cacheObject = True
            , rawRead = readFile
            , rawWrite = writeFile
            }
    pure $
      defaultCompiler
        { withIR =
            \ModSummary {..} IR {..} ->
              liftIO $ do
                objectWrite ms_mod $ ppShow cmmRaw
                s <- objectRead ms_mod
                print (ms_mod, length s)
        }
