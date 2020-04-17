{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.FrontendPlugin
  ( frontendPlugin,
  )
where

import Asterius.Binary.File
import Asterius.BuildInfo
import Asterius.CodeGen
import Asterius.Foreign.DsForeign
import Asterius.Foreign.TcForeign
import Asterius.GHCi.Internals
import Asterius.JSFFI
import Asterius.TypesConv
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import qualified GHC
import qualified GhcPlugins as GHC
import qualified Hooks as GHC
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.FrontendPlugin
import Language.Haskell.GHC.Toolkit.Orphans.Show ()
import qualified Stream
import System.Environment.Blank
import System.FilePath

frontendPlugin :: GHC.FrontendPlugin
frontendPlugin = makeFrontendPlugin $ do
  is_debug <- liftIO $ isJust <$> getEnv "ASTERIUS_DEBUG"
  do
    dflags <- GHC.getSessionDynFlags
    void $
      GHC.setSessionDynFlags
        dflags
          { GHC.hooks =
              (GHC.hooks dflags)
                { GHC.dsForeignsHook = Just asteriusDsForeigns,
                  GHC.tcForeignImportsHook = Just asteriusTcForeignImports,
                  GHC.tcForeignExportsHook = Just asteriusTcForeignExports,
                  GHC.hscCompileCoreExprHook = Just asteriusHscCompileCoreExpr,
                  GHC.startIServHook = Just asteriusStartIServ,
                  GHC.iservCallHook = Just asteriusIservCall,
                  GHC.readIServHook = Just asteriusReadIServ,
                  GHC.writeIServHook = Just asteriusWriteIServ,
                  GHC.stopIServHook = Just asteriusStopIServ
                }
          }
  do
    dflags <- GHC.getSessionDynFlags
    void
      $ GHC.setSessionDynFlags
      $ dflags
        { GHC.settings =
            (GHC.settings dflags)
              { GHC.sPgm_L = unlit,
                GHC.sPgm_l = (ahcLd, []),
                GHC.sPgm_i = "false"
              }
        }
        `GHC.gopt_set` GHC.Opt_EagerBlackHoling
        `GHC.gopt_set` GHC.Opt_ExternalInterpreter
  when is_debug $ do
    dflags <- GHC.getSessionDynFlags
    void
      $ GHC.setSessionDynFlags
      $ dflags
        `GHC.gopt_set` GHC.Opt_DoCoreLinting
        `GHC.gopt_set` GHC.Opt_DoStgLinting
        `GHC.gopt_set` GHC.Opt_DoCmmLinting
  pure $
    mempty
      { withHaskellIR = \GHC.ModSummary {..} ir@HaskellIR {..} obj_path -> do
          dflags <- GHC.getDynFlags
          liftIO $ do
            ffi_mod <- getFFIModule dflags ms_mod
            runCodeGen (marshalHaskellIR ms_mod ir) dflags ms_mod >>= \case
              Left err -> throwIO err
              Right m' -> do
                let m = ffi_mod <> m'
                putFile obj_path m
                when is_debug $ do
                  let p = (obj_path -<.>)
                  writeFile (p "dump-wasm-ast") $ show m
                  cmm_raw <- Stream.collect cmmRaw
                  writeFile (p "dump-cmm-raw-ast") $ show cmm_raw
                  asmPrint dflags (p "dump-cmm-raw") cmm_raw,
        withCmmIR = \ir@CmmIR {..} obj_path -> do
          dflags <- GHC.getDynFlags
          let ms_mod =
                GHC.Module GHC.rtsUnitId $ GHC.mkModuleName $
                  takeBaseName
                    obj_path
          liftIO $
            runCodeGen (marshalCmmIR ms_mod ir) dflags ms_mod >>= \case
              Left err -> throwIO err
              Right m -> do
                putFile obj_path m
                when is_debug $ do
                  let p = (obj_path -<.>)
                  writeFile (p "dump-wasm-ast") $ show m
                  cmm_raw <- Stream.collect cmmRaw
                  writeFile (p "dump-cmm-raw-ast") $ show cmm_raw
                  asmPrint dflags (p "dump-cmm-raw") cmm_raw
      }
