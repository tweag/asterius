{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.FrontendPlugin
  ( frontendPlugin
  ) where

import CLabel
import Cmm
import Control.Monad.Except
import Data.Maybe
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
            , objectExt = "txt"
            , cacheObject = True
            , rawRead = readFile
            , rawWrite = writeFile
            }
    pure $
      defaultCompiler
        { withIR =
            \ModSummary {..} IR {..} ->
              let clbls =
                    [ case decl of
                      CmmData (Section _ clbl) _ -> clbl
                      CmmProc _ clbl _ _ -> clbl
                    | decl <- cmmRaw
                    ]
                  ns = mapMaybe (fmap nameStableString . hasHaskellName) clbls
               in liftIO $ objectWrite ms_mod $ ppShow (clbls, ns)
        }
