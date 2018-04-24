{-# LANGUAGE RecordWildCards #-}

module Asterius.FrontendPlugin
  ( frontendPlugin
  ) where

import Asterius.CodeGen
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Serialize
import GhcPlugins
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.FrontendPlugin
import System.Environment
import Text.Show.Pretty

frontendPlugin :: FrontendPlugin
frontendPlugin =
  makeFrontendPlugin $
  liftIO $ do
    obj_topdir <- getEnv "ASTERIUS_LIB_DIR"
    is_debug <- isJust <$> lookupEnv "ASTERIUS_DEBUG"
    pure $
      defaultCompiler
        { withHaskellIR =
            \ModSummary {..} ir@HaskellIR {..} -> do
              let mod_sym = marshalToModuleSymbol ms_mod
              dflags <- getDynFlags
              liftIO $
                case runCodeGen (marshalHaskellIR ir) dflags ms_mod of
                  Left err -> throwIO err
                  Right m -> do
                    p <- moduleSymbolPath obj_topdir mod_sym "asterius_o"
                    BS.writeFile p $ encode m
                    when is_debug $ do
                      p_a <- moduleSymbolPath obj_topdir mod_sym "txt"
                      writeFile p_a $ ppShow m
                      p_c <-
                        moduleSymbolPath obj_topdir mod_sym "dump-cmm-raw-ast"
                      writeFile p_c $ ppShow cmmRaw
        }
