{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Boot
  ( BootArgs (..),
    defaultBootArgs,
    boot
    )
where

import Asterius.BuildInfo
import Asterius.Builtins
import Asterius.CodeGen
import Asterius.Internals
import Asterius.Internals.Directory
import Asterius.TypesConv
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Maybe
import qualified DynFlags as GHC
import qualified GHC
import Language.Haskell.GHC.Toolkit.BuildInfo
  ( bootLibsPath,
    sandboxGhcLibDir
    )
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.Orphans.Show
import Language.Haskell.GHC.Toolkit.Run
  ( defaultConfig,
    ghcFlags,
    runCmm
    )
import qualified Module as GHC
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO hiding (IO)
import System.Process
import Prelude hiding (IO)

data BootArgs
  = BootArgs
      { bootDir :: FilePath,
        configureOptions, buildOptions, installOptions :: String,
        builtinsOptions :: BuiltinsOptions
        }

defaultBootArgs :: BootArgs
defaultBootArgs = BootArgs
  { bootDir = dataDir </> ".boot",
    configureOptions = "--disable-shared --disable-profiling --disable-debug-info --disable-library-for-ghci --disable-split-objs --disable-split-sections --disable-library-stripping -O2 --ghc-option=-v1 --ghc-option=-dsuppress-ticks",
    buildOptions = "",
    installOptions = "",
    builtinsOptions = defaultBuiltinsOptions
    }

bootTmpDir :: BootArgs -> FilePath
bootTmpDir BootArgs {..} = bootDir </> "dist"

bootCreateProcess :: BootArgs -> IO CreateProcess
bootCreateProcess args@BootArgs {..} = do
  e <- getEnvironment
  pure
    (proc "sh" ["-e", "boot.sh"])
      { cwd = Just dataDir,
        env = Just
          $ ("ASTERIUS_BOOT_LIBS_DIR", bootLibsPath)
          : ("ASTERIUS_SANDBOX_GHC_LIBDIR", sandboxGhcLibDir)
          : ("ASTERIUS_LIB_DIR", bootDir </> "asterius_lib")
          : ("ASTERIUS_TMP_DIR", bootTmpDir args)
          : ("ASTERIUS_GHC", ghc)
          : ("ASTERIUS_GHCLIBDIR", ghcLibDir)
          : ("ASTERIUS_AHC", ahc)
          : ("ASTERIUS_AHCPKG", ahcPkg)
          : ("ASTERIUS_CONFIGURE_OPTIONS", configureOptions)
          : ("ASTERIUS_BUILD_OPTIONS", buildOptions)
          : ("ASTERIUS_INSTALL_OPTIONS", installOptions)
          : [(k, v) | (k, v) <- e, k /= "GHC_PACKAGE_PATH"],
        delegate_ctlc = True
        }

bootRTSCmm :: BootArgs -> IO ()
bootRTSCmm BootArgs {..} =
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut
    $ GHC.runGhc (Just obj_topdir)
    $ do
      dflags0 <- GHC.getSessionDynFlags
      _ <-
        GHC.setSessionDynFlags
          $ GHC.setGeneralFlag' GHC.Opt_SuppressTicks dflags0
      dflags <- GHC.getSessionDynFlags
      setDynFlagsRef dflags
      is_debug <- isJust <$> liftIO (lookupEnv "ASTERIUS_DEBUG")
      obj_paths_ref <- liftIO $ newIORef []
      cmm_files <-
        liftIO
          $ fmap (filter ((== ".cmm") . takeExtension))
          $ listFilesRecursive
          $ takeDirectory rts_path
      runCmm
        defaultConfig
          { ghcFlags = [ "-this-unit-id",
                         "rts",
                         "-dcmm-lint",
                         "-O2",
                         "-DASTERIUS",
                         "-optc=-DASTERIUS",
                         "-I" <> obj_topdir </> "include"
                         ]
            }
        cmm_files
        ( \obj_path ir@CmmIR {..} ->
            let ms_mod =
                  ( GHC.Module GHC.rtsUnitId $ GHC.mkModuleName
                      $ takeBaseName
                          obj_path
                    )
             in case runCodeGen (marshalCmmIR ms_mod ir) dflags ms_mod of
                  Left err -> throwIO err
                  Right m -> do
                    encodeFile obj_path m
                    modifyIORef' obj_paths_ref (obj_path :)
                    when is_debug $ do
                      let p = (obj_path -<.>)
                      writeFile (p "dump-wasm-ast") $ show m
                      writeFile (p "dump-cmm-raw-ast") $ show cmmRaw
                      asmPrint dflags (p "dump-cmm-raw") cmmRaw
          )
      liftIO $ do
        obj_paths <- readIORef obj_paths_ref
        tmpdir <- getTemporaryDirectory
        (rsp_path, rsp_h) <- openTempFile tmpdir "ar.rsp"
        hPutStr rsp_h $ unlines obj_paths
        hClose rsp_h
        callProcess
          "ar"
          ["-r", "-c", obj_topdir </> "rts" </> "libHSrts.a", '@' : rsp_path]
        removeFile rsp_path
  where
    rts_path = bootLibsPath </> "rts"
    obj_topdir = bootDir </> "asterius_lib"

runBootCreateProcess :: CreateProcess -> IO ()
runBootCreateProcess = flip withCreateProcess $ \_ _ _ ph -> do
  ec <- waitForProcess ph
  case ec of
    ExitFailure _ -> fail "boot failure"
    _ -> pure ()

boot :: BootArgs -> IO ()
boot args = do
  cp_boot <- bootCreateProcess args
  runBootCreateProcess
    cp_boot
      { cmdspec = RawCommand "sh" ["-e", "boot-init.sh"]
        }
  bootRTSCmm args
  runBootCreateProcess cp_boot
  is_debug <- isJust <$> lookupEnv "ASTERIUS_DEBUG"
  unless is_debug $ removePathForcibly $ bootTmpDir args
