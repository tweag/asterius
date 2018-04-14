{-# LANGUAGE RecordWildCards #-}

module Asterius.FrontendPlugin
  ( frontendPlugin
  ) where

import Asterius.CodeGen
import Asterius.Types
import Control.Monad
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Serialize
import GhcPlugins
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.FrontendPlugin
import System.FilePath
import Text.Show.Pretty
import UnliftIO
import UnliftIO.Directory
import UnliftIO.Environment

frontendPlugin :: FrontendPlugin
frontendPlugin =
  makeFrontendPlugin $
  liftIO $ do
    obj_topdir <- getEnv "ASTERIUS_LIB_DIR"
    is_debug <- isJust <$> lookupEnv "ASTERIUS_DEBUG"
    let sym_db_path = obj_topdir </> ".asterius_sym_db"
    sym_db_ref <- newIORef AsteriusSymbolDB {symbolMap = mempty}
    void $
      tryAnyDeep $ do
        sym_db_r <- decode <$> BS.readFile sym_db_path
        case sym_db_r of
          Left err -> throwString err
          Right sym_db -> writeIORef sym_db_ref sym_db
    pure $
      defaultCompiler
        { withHaskellIR =
            \ModSummary {..} ir@HaskellIR {..} -> do
              let mod_sym = marshalToModuleSymbol ms_mod
              dflags <- getDynFlags
              liftIO $ do
                m <- marshalHaskellIR dflags ir
                atomicModifyIORef' sym_db_ref $ \sym_db ->
                  (updateSymbolDB mod_sym m sym_db, ())
                p <- moduleSymbolPath obj_topdir mod_sym "asterius_o"
                BS.writeFile p $ encode m
                when is_debug $ do
                  p_a <- moduleSymbolPath obj_topdir mod_sym "txt"
                  writeFile p_a $ ppShow m
                  p_c <- moduleSymbolPath obj_topdir mod_sym "dump-cmm-raw-ast"
                  writeFile p_c $ ppShow cmmRaw
        , finalize =
            liftIO $ do
              createDirectoryIfMissing True obj_topdir
              sym_db <- readIORef sym_db_ref
              BS.writeFile sym_db_path $ encode sym_db
              when is_debug $
                writeFile (obj_topdir </> "asterius_sym_db.txt") $ ppShow sym_db
        }
