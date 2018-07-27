{-# LANGUAGE RecordWildCards #-}

module Asterius.FrontendPlugin
  ( frontendPlugin
  ) where

import Asterius.CodeGen
import Asterius.JSFFI
import Asterius.Store
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Maybe
import qualified GhcPlugins as GHC
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.FrontendPlugin
import System.Environment
import System.FilePath
import Text.Show.Pretty

frontendPlugin :: GHC.FrontendPlugin
frontendPlugin =
  makeFrontendPlugin $
  liftIO $ do
    obj_topdir <- getEnv "ASTERIUS_LIB_DIR"
    is_debug <- isJust <$> lookupEnv "ASTERIUS_DEBUG"
    store_ref <- decodeStore (obj_topdir </> "asterius_store") >>= newIORef
    get_ffi_mod_ref <- newIORef $ error "get_ffi_mod_ref not initialized"
    (c, get_ffi_mod) <-
      addFFIProcessor
        mempty
          { withHaskellIR =
              \GHC.ModSummary {..} ir@HaskellIR {..} -> do
                let mod_sym = marshalToModuleSymbol ms_mod
                dflags <- GHC.getDynFlags
                liftIO $
                  case runCodeGen (marshalHaskellIR ir) dflags ms_mod of
                    Left err -> throwIO err
                    Right m' -> do
                      get_ffi_mod <- readIORef get_ffi_mod_ref
                      ffi_mod <- get_ffi_mod mod_sym
                      let m = ffi_mod <> m'
                      encodeAsteriusModule obj_topdir mod_sym m
                      atomicModifyIORef' store_ref $ \store ->
                        (registerModule obj_topdir mod_sym m store, ())
                      when is_debug $ do
                        let p_c =
                              asteriusModulePath
                                obj_topdir
                                mod_sym
                                "dump-cmm-raw-ast"
                        writeFile p_c $ ppShow cmmRaw
                        let p_s = asteriusModulePath obj_topdir mod_sym "txt"
                        writeFile p_s $ ppShow m
          , finalize =
              liftIO $ do
                store <- readIORef store_ref
                encodeStore (obj_topdir </> "asterius_store") store
          }
    writeIORef get_ffi_mod_ref get_ffi_mod
    pure c
