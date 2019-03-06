{-# LANGUAGE RecordWildCards #-}

module Asterius.FrontendPlugin
  ( frontendPlugin
  ) where

import Asterius.CodeGen
import Asterius.Internals
import qualified Asterius.Iserv.CompileCoreExpr as Iserv
import Asterius.JSFFI
import Asterius.TypesConv
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Maybe
import qualified GHC
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
    is_debug <- isJust <$> getEnv "ASTERIUS_DEBUG"
    get_ffi_mod_ref <- newIORef $ error "get_ffi_mod_ref not initialized"
    us_ref <- GHC.mkSplitUniqSupply 'A' >>= newIORef
    (c, get_ffi_mod) <-
      addFFIProcessor
        mempty
          { compileCoreExpr = Just $ Iserv.compileCoreExpr us_ref
          , withHaskellIR =
              \GHC.ModSummary {..} ir@HaskellIR {..} obj_path -> do
                dflags <- GHC.getDynFlags
                setDynFlagsRef dflags
                let mod_sym = marshalToModuleSymbol ms_mod
                liftIO $
                  case runCodeGen (marshalHaskellIR ms_mod ir) dflags ms_mod of
                    Left err -> throwIO err
                    Right m' -> do
                      get_ffi_mod <- readIORef get_ffi_mod_ref
                      ffi_mod <- get_ffi_mod mod_sym
                      let m = ffi_mod <> m'
                      encodeFile obj_path m
                      when is_debug $ do
                        let p = (obj_path -<.>)
                        writeFile (p "dump-wasm-ast") $ show m
                        writeFile (p "dump-cmm-raw-ast") $ show cmmRaw
                        asmPrint dflags (p "dump-cmm-raw") cmmRaw
                        writeFile (p "dump-cmm-ast") $ show cmm
                        asmPrint dflags (p "dump-cmm") cmm
                        writeFile (p "dump-stg-ast") $ show stg
                        asmPrint dflags (p "dump-stg") stg
                        writeFile (p "dump-core-ast") $ show core
                        asmPrint dflags (p "dump-core") $ GHC.cg_binds core
          , withCmmIR =
              \ir@CmmIR {..} obj_path -> do
                dflags <- GHC.getDynFlags
                setDynFlagsRef dflags
                let ms_mod =
                      GHC.Module GHC.rtsUnitId $
                      GHC.mkModuleName $ takeBaseName obj_path
                liftIO $
                  case runCodeGen (marshalCmmIR ms_mod ir) dflags ms_mod of
                    Left err -> throwIO err
                    Right m -> do
                      encodeFile obj_path m
                      when is_debug $ do
                        let p = (obj_path -<.>)
                        writeFile (p "dump-wasm-ast") $ show m
                        writeFile (p "dump-cmm-raw-ast") $ show cmmRaw
                        asmPrint dflags (p "dump-cmm-raw") cmmRaw
                        writeFile (p "dump-cmm-ast") $ show cmm
                        asmPrint dflags (p "dump-cmm") cmm
          }
    writeIORef get_ffi_mod_ref get_ffi_mod
    pure c
