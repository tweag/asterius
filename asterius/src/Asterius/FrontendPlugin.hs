{-# LANGUAGE RecordWildCards #-}

module Asterius.FrontendPlugin
  ( frontendPlugin
  ) where

import Asterius.CodeGen
import qualified Data.ByteString as BS
import Data.Serialize
import GhcPlugins
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.FrontendPlugin
import UnliftIO.Environment

frontendPlugin :: FrontendPlugin
frontendPlugin =
  frontendPluginFromCompiler $ do
    obj_topdir <- getEnv "ASTERIUS_LIB_DIR"
    pure $
      defaultCompiler
        { withIR =
            \ModSummary {..} ir -> do
              dflags <- getDynFlags
              liftIO $ do
                m <- marshalIR dflags ir
                p <- modulePath obj_topdir ms_mod "asterius_o"
                BS.writeFile p $ encode m
        }
