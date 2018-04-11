{-# LANGUAGE RecordWildCards #-}

module Asterius.FrontendPlugin
  ( frontendPlugin
  ) where

import Asterius.CodeGen
import qualified Data.ByteString as BS
import Data.Functor
import Data.Serialize
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
            , globalFile = ""
            , cacheObject = False
            , rawRead =
                \p -> do
                  e <- decode <$> BS.readFile p
                  case e of
                    Left err -> fail err
                    Right r -> pure r
            , rawWrite = \p m -> BS.writeFile p $ encode m
            }
    pure $
      defaultCompiler
        { withIR =
            \ModSummary {..} ir -> do
              dflags <- getDynFlags
              liftIO $ do
                m <- marshalIR dflags ir
                objectWrite ms_mod m
                void $ objectRead ms_mod
        }
