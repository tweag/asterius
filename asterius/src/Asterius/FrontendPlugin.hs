{-# LANGUAGE RecordWildCards #-}

module Asterius.FrontendPlugin
  ( frontendPlugin,
  )
where

import Asterius.Binary.File
import Asterius.CodeGen
import Asterius.Foreign.DsForeign
import Asterius.Foreign.TcForeign
import Asterius.GHCi.Internals
import Asterius.Internals.PrettyShow
import Asterius.JSFFI
import Asterius.Types
import Asterius.TypesConv
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Tuple
import qualified GHC
import qualified GhcMonad as GHC
import qualified GhcPlugins as GHC
import qualified Hooks as GHC
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.Hooks
import Language.Haskell.GHC.Toolkit.Orphans.Show
  (
  )
import qualified Stream
import System.Environment.Blank
import System.FilePath
import qualified ToolSettings as GHC

frontendPlugin :: GHC.Ghc ()
frontendPlugin = do
  is_debug <- liftIO $ isJust <$> getEnv "ASTERIUS_DEBUG"
  do
    dflags <- GHC.getSessionDynFlags
    mySetSessionDynFlags
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
    mySetSessionDynFlags $
      dflags
        { GHC.toolSettings =
            (GHC.toolSettings dflags)
              { GHC.toolSettings_pgm_L = "unlit",
                GHC.toolSettings_pgm_l = ("ahc-ld", []),
                GHC.toolSettings_pgm_i = "false"
              }
        }
        `GHC.gopt_set` GHC.Opt_ExternalInterpreter
  when is_debug $ do
    dflags <- GHC.getSessionDynFlags
    mySetSessionDynFlags $
      dflags
        `GHC.gopt_set` GHC.Opt_DoCoreLinting
        `GHC.gopt_set` GHC.Opt_DoStgLinting
        `GHC.gopt_set` GHC.Opt_DoCmmLinting
  do
    dflags <- GHC.getSessionDynFlags
    h' <-
      liftIO $
        hooksFromCompiler
          ( Compiler
              { withCmmIR = \dflags this_mod ir@CmmIR {..} obj_path -> do
                  ffi_mod <- getFFIModule dflags this_mod
                  m' <- runCodeGen
                    ( marshalCmmIR this_mod ir
                    )
                    dflags
                    this_mod
                  let m = ffi_mod <> m'
                  putFile obj_path $ toCachedModule m
                  when is_debug $ do
                          let p = (obj_path -<.>)
                          writeFile (p "dump-wasm-ast") =<< prettyShow m
                          cmm_raw <- Stream.collect cmmRaw
                          writeFile (p "dump-cmm-raw-ast") =<< prettyShow cmm_raw
                          asmPrint dflags (p "dump-cmm-raw") cmm_raw
              }
          )
          (GHC.hooks dflags)
    mySetSessionDynFlags
      dflags
        { GHC.integerLibrary = GHC.IntegerSimple,
          GHC.hooks = h'
        }

mySetSessionDynFlags :: GHC.DynFlags -> GHC.Ghc ()
mySetSessionDynFlags dflags = GHC.modifySession $ \h ->
  h
    { GHC.hsc_dflags = dflags,
      GHC.hsc_IC = (GHC.hsc_IC h) {GHC.ic_dflags = dflags}
    }
