{-# LANGUAGE RecordWildCards #-}

module Asterius.FrontendPlugin
  ( frontendPlugin
  ) where

import Asterius.CodeGen
import Control.Monad
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Serialize
import GhcPlugins
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.FrontendPlugin
import System.FilePath
import System.IO.Unsafe
import Text.Show.Pretty
import UnliftIO
import UnliftIO.Directory
import UnliftIO.Environment

{-# NOINLINE symDBRef #-}
symDBRef :: IORef AsteriusSymbolDatabase
symDBRef =
  unsafePerformIO $ newIORef AsteriusSymbolDatabase {symbolMap = mempty}

{-# NOINLINE symDBFinRef #-}
symDBFinRef :: IORef (IO ())
symDBFinRef = unsafePerformIO $ newIORef $ pure ()

frontendPlugin :: FrontendPlugin
frontendPlugin =
  frontendPluginFromCompiler
    (liftIO $ do
       obj_topdir <- getEnv "ASTERIUS_LIB_DIR"
       is_debug <- isJust <$> lookupEnv "ASTERIUS_DEBUG"
       let sym_db_path = obj_topdir </> ".asterius_sym_db"
       void $
         tryAnyDeep $ do
           sym_db_r <- decode <$> BS.readFile sym_db_path
           case sym_db_r of
             Left err -> throwString err
             Right sym_db -> writeIORef symDBRef sym_db
       writeIORef symDBFinRef $ do
         createDirectoryIfMissing True obj_topdir
         sym_db <- readIORef symDBRef
         BS.writeFile sym_db_path $ encode sym_db
         when is_debug $
           writeFile (obj_topdir </> "asterius_sym_db.txt") $ ppShow sym_db
       pure $
         defaultCompiler
           { withIR =
               \ModSummary {..} ir@IR {..} -> do
                 let mod_sym = marshalToModuleSymbol ms_mod
                 dflags <- getDynFlags
                 liftIO $ do
                   m <- marshalIR dflags ir
                   p <- moduleSymbolPath obj_topdir mod_sym "asterius_o"
                   m' <-
                     atomicModifyIORef' symDBRef $ \sym_db ->
                       let (m', sym_db') = chaseModule sym_db mod_sym m
                        in (sym_db', m')
                   BS.writeFile p $ encode m'
                   when is_debug $ do
                     p_a <- moduleSymbolPath obj_topdir mod_sym "txt"
                     writeFile p_a $ ppShow m'
                     p_c <-
                       moduleSymbolPath obj_topdir mod_sym "dump-cmm-raw-ast"
                     writeFile p_c $ ppShow cmmRaw
           })
    (readIORef symDBFinRef >>= liftIO)
