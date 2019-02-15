{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Boot
  ( BootArgs(..)
  , defaultBootArgs
  , boot
  ) where

import Asterius.BuildInfo
import Asterius.Builtins
import Asterius.CodeGen
import Asterius.Internals
import Asterius.Internals.Codensity
import Asterius.Store
import Asterius.TypesConv
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.IORef
import Data.Maybe
import qualified DynFlags as GHC
import qualified GHC
import Language.Haskell.GHC.Toolkit.BuildInfo (bootLibsPath, sandboxGhcLibDir)
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.Orphans.Show
import Language.Haskell.GHC.Toolkit.Run (defaultConfig, ghcFlags, runCmm)
import qualified Module as GHC
import Prelude hiding (IO)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process

data BootArgs = BootArgs
  { bootDir :: FilePath
  , configureOptions, buildOptions, installOptions :: String
  , builtinsOptions :: BuiltinsOptions
  }

defaultBootArgs :: BootArgs
defaultBootArgs =
  BootArgs
    { bootDir = dataDir </> ".boot"
    , configureOptions =
        "--disable-shared --disable-profiling --disable-debug-info --disable-library-for-ghci --disable-split-objs --disable-split-sections --disable-library-stripping -O2 --ghc-option=-v1 --with-ar=" <>
        ahcAr
    , buildOptions = ""
    , installOptions = ""
    , builtinsOptions = defaultBuiltinsOptions
    }

bootTmpDir :: BootArgs -> FilePath
bootTmpDir BootArgs {..} = bootDir </> "dist"

bootCreateProcess :: BootArgs -> IO CreateProcess
bootCreateProcess args@BootArgs {..} = do
  e <- getEnvironment
  pure
    (proc "sh" ["-e", "boot.sh"])
      { cwd = Just dataDir
      , env =
          Just $
          ("ASTERIUS_BOOT_LIBS_DIR", bootLibsPath) :
          ("ASTERIUS_SANDBOX_GHC_LIBDIR", sandboxGhcLibDir) :
          ("ASTERIUS_LIB_DIR", bootDir </> "asterius_lib") :
          ("ASTERIUS_TMP_DIR", bootTmpDir args) :
          ("ASTERIUS_GHC", ghc) :
          ("ASTERIUS_GHCLIBDIR", ghcLibDir) :
          ("ASTERIUS_GHCPKG", ghcPkg) :
          ("ASTERIUS_AHC", ahc) :
          ("ASTERIUS_CONFIGURE_OPTIONS", configureOptions) :
          ("ASTERIUS_BUILD_OPTIONS", buildOptions) :
          ("ASTERIUS_INSTALL_OPTIONS", installOptions) :
          [(k, v) | (k, v) <- e, k /= "GHC_PACKAGE_PATH"]
      , delegate_ctlc = True
      }

bootRTSCmm :: BootArgs -> IO ()
bootRTSCmm BootArgs {..} =
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
  GHC.runGhc (Just obj_topdir) $
  lowerCodensity $ do
    dflags <- lift GHC.getSessionDynFlags
    setDynFlagsRef dflags
    is_debug <- isJust <$> liftIO (lookupEnv "ASTERIUS_DEBUG")
    store_ref <- liftIO $ newIORef mempty
    rts_cmm_mods <-
      map takeBaseName . filter ((== ".cmm") . takeExtension) <$>
      liftIO (listDirectory rts_path)
    lift
      (runCmm
         defaultConfig
           { ghcFlags =
               [ "-this-unit-id"
               , "rts"
               , "-dcmm-lint"
               , "-O2"
               , "-I" <> obj_topdir </> "include"
               ]
           }
         [rts_path </> m <.> "cmm" | m <- rts_cmm_mods]
         (\obj_path ir@CmmIR {..} ->
            let ms_mod =
                  (GHC.Module GHC.rtsUnitId $
                   GHC.mkModuleName $ takeBaseName obj_path)
                mod_sym = marshalToModuleSymbol ms_mod
             in case runCodeGen (marshalCmmIR ir) dflags ms_mod of
                  Left err -> throwIO err
                  Right m -> do
                    encodeAsteriusModule obj_topdir mod_sym m
                    modifyIORef' store_ref $ registerModule obj_topdir mod_sym m
                    when is_debug $ do
                      let p = (obj_path -<.>)
                      writeFile (p "dump-wasm-ast") $ show m
                      writeFile (p "dump-cmm-raw-ast") $ show cmmRaw
                      asmPrint dflags (p "dump-cmm-raw") cmmRaw
                      writeFile (p "dump-cmm-ast") $ show cmm
                      asmPrint dflags (p "dump-cmm") cmm))
    liftIO $ do
      store <- readIORef store_ref
      encodeStore store_path store
  where
    rts_path = bootLibsPath </> "rts"
    obj_topdir = bootDir </> "asterius_lib"
    store_path = obj_topdir </> "asterius_store"

runBootCreateProcess :: CreateProcess -> IO ()
runBootCreateProcess =
  flip withCreateProcess $ \_ _ _ ph -> do
    ec <- waitForProcess ph
    case ec of
      ExitFailure _ -> fail "boot failure"
      _ -> pure ()

boot :: BootArgs -> IO ()
boot args = do
  cp_boot <- bootCreateProcess args
  runBootCreateProcess
    cp_boot {cmdspec = RawCommand "sh" ["-e", "boot-init.sh"]}
  bootRTSCmm args
  runBootCreateProcess cp_boot
  is_debug <- isJust <$> lookupEnv "ASTERIUS_DEBUG"
  unless is_debug $ removePathForcibly $ bootTmpDir args
