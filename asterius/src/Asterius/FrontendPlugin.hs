{-# LANGUAGE RecordWildCards #-}

module Asterius.FrontendPlugin
  ( frontendPlugin
  ) where

import Asterius.CodeGen
import Data.Compact
import Data.Compact.Serialize
import Data.Functor
import GhcPlugins
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.FrontendPlugin
import Language.Haskell.GHC.Toolkit.ObjectStore
import System.Environment

frontendPlugin :: FrontendPlugin
frontendPlugin =
  frontendPluginFromCompiler $ do
    Store {..} <-
      liftIO $ do
        obj_topdir <- getEnv "ASTERIUS_LIB_DIR"
        newStore
          StoreConfig
            { storeTopDir = obj_topdir
            , objectExt = "asterius_o"
            , cacheObject = False
            , rawRead =
                \p -> do
                  e <- unsafeReadCompact p
                  case e of
                    Left err -> fail err
                    Right r -> pure r
            , rawWrite = writeCompact
            }
    pure $
      defaultCompiler
        { withIR =
            \ModSummary {..} ir -> do
              dflags <- getDynFlags
              liftIO $ do
                m <- marshalIR dflags ir
                c <- compactWithSharing m
                objectWrite ms_mod c
                void $ objectRead ms_mod
        }
