{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.FrontendPlugin
  ( frontendPlugin
  ) where

import Asterius.IR
import Control.Monad.Except
import Control.Monad.Reader
import GHC
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
            \mod_summary@ModSummary {..} ir ->
              liftIO $
              objectWrite ms_mod $
              ppShow $
              runExcept $
              flip runReaderT defaultMarshalContext $ marshalIR mod_summary ir
        }
