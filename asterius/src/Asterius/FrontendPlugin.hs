{-# LANGUAGE RecordWildCards #-}

module Asterius.FrontendPlugin
  ( frontendPlugin
  ) where

import Asterius.CodeGen
import Asterius.JSFFI
import Asterius.Store
import Asterius.TypesConv
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Maybe
import qualified GhcPlugins as GHC
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.FrontendPlugin
import Language.Haskell.GHC.Toolkit.Orphans.Show
import System.Environment.Blank
import System.FilePath

frontendPlugin :: GHC.FrontendPlugin
frontendPlugin =
  makeFrontendPlugin $
  liftIO $ do
    m_obj_topdir <- getEnv "ASTERIUS_LIB_DIR"
    is_debug <- isJust <$> getEnv "ASTERIUS_DEBUG"
    store_ref <-
      (case m_obj_topdir of
         Just obj_topdir -> decodeStore (obj_topdir </> "asterius_store")
         _ -> pure $ error "Asterius.FrontendPlugin.frontendPlugin") >>=
      newIORef
    get_ffi_mod_ref <- newIORef $ error "get_ffi_mod_ref not initialized"
    (c, get_ffi_mod) <-
      addFFIProcessor
        mempty
          { withHaskellIR =
              \GHC.ModSummary {..} ir@HaskellIR {..} -> do
                let mod_sym = marshalToModuleSymbol ms_mod
                dflags <- GHC.getDynFlags
                setDynFlagsRef dflags
                liftIO $
                  case runCodeGen (marshalHaskellIR ir) dflags ms_mod of
                    Left err -> throwIO err
                    Right m' -> do
                      get_ffi_mod <- readIORef get_ffi_mod_ref
                      ffi_mod <- get_ffi_mod mod_sym
                      let m = ffi_mod <> m'
                      case m_obj_topdir of
                        Just obj_topdir -> do
                          encodeAsteriusModule obj_topdir mod_sym m
                          atomicModifyIORef' store_ref $ \store ->
                            (registerModule obj_topdir mod_sym m store, ())
                          when is_debug $ do
                            let p = asteriusModulePath obj_topdir mod_sym
                            writeFile (p "dump-wasm-ast") $ show m
                            writeFile (p "dump-cmm-raw-ast") $ show cmmRaw
                            asmPrint dflags (p "dump-cmm-raw") cmmRaw
                            writeFile (p "dump-cmm-ast") $ show cmm
                            asmPrint dflags (p "dump-cmm") cmm
                            writeFile (p "dump-stg-ast") $ show stg
                            asmPrint dflags (p "dump-stg") stg
                            writeFile (p "dump-core-ast") $ show core
                            asmPrint dflags (p "dump-core") $ GHC.cg_binds core
                        _ -> pure ()
          , finalize =
              case m_obj_topdir of
                Just obj_topdir ->
                  liftIO $ do
                    store <- readIORef store_ref
                    encodeStore (obj_topdir </> "asterius_store") store
                _ -> pure ()
          }
    writeIORef get_ffi_mod_ref get_ffi_mod
    pure c
