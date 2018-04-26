{-# LANGUAGE RecordWildCards #-}

module Asterius.FrontendPlugin
  ( frontendPlugin
  ) where

import Asterius.CodeGen
import Asterius.Internals
import Asterius.SymbolDB
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Maybe
import GhcPlugins
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.FrontendPlugin
import System.Environment
import System.FilePath
import Text.Show.Pretty

frontendPlugin :: FrontendPlugin
frontendPlugin =
  makeFrontendPlugin $
  liftIO $ do
    obj_topdir <- getEnv "ASTERIUS_LIB_DIR"
    is_debug <- isJust <$> lookupEnv "ASTERIUS_DEBUG"
    init_sym_db <- decodeFile $ obj_topdir </> "asterius_sym_db"
    sym_db_ref <- newIORef init_sym_db
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
                    encodeFile p m
                    when is_debug $ do
                      p_a <- moduleSymbolPath obj_topdir mod_sym "txt"
                      writeFile p_a $ ppShow m
                      p_c <-
                        moduleSymbolPath obj_topdir mod_sym "dump-cmm-raw-ast"
                      writeFile p_c $ ppShow cmmRaw
                    atomicModifyIORef' sym_db_ref $ \sym_db ->
                      (enrichSymbolDB mod_sym m sym_db, ())
        , finalize =
            liftIO $ do
              sym_db <- readIORef sym_db_ref
              encodeFile (obj_topdir </> "asterius_sym_db") sym_db
              when is_debug $
                writeFile (obj_topdir </> "asterius_sym_db.txt") $ ppShow sym_db
        }
