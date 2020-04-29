{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Boot
  ( BootArgs (..),
    getDefaultBootArgs,
    boot,
  )
where

import Asterius.Binary.File
import Asterius.BuildInfo
import Asterius.Builtins
import Asterius.CodeGen
import Asterius.Internals.Directory
import Asterius.Types
import Asterius.TypesConv
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Maybe
import qualified DynFlags as GHC
import qualified GHC
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.Orphans.Show ()
import Language.Haskell.GHC.Toolkit.Run
  ( defaultConfig,
    ghcFlags,
    runCmm,
  )
import qualified Paths_asterius
import qualified Module as GHC
import qualified Stream
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

data BootArgs
  = BootArgs
      { bootDir :: FilePath,
        configureOptions :: String,
        builtinsOptions :: BuiltinsOptions
      }

getDefaultBootArgs :: BootArgs
getDefaultBootArgs = BootArgs
  { bootDir = rootBootDir </> ".boot",
    configureOptions =
      "--disable-shared\
      \ --disable-profiling\
      \ --disable-debug-info\
      \ --disable-library-for-ghci\
      \ --disable-split-objs\
      \ --disable-split-sections\
      \ --disable-library-stripping\
      \ -O2\
      \ --ghc-option=-v1\
      \ --ghc-option=-dsuppress-ticks",
    builtinsOptions = defaultBuiltinsOptions
  }

bootTmpDir :: BootArgs -> FilePath
bootTmpDir BootArgs {..} = bootDir </> "dist"

bootCreateProcess :: BootArgs -> IO CreateProcess
bootCreateProcess args@BootArgs {..} = do
  e <- getEnvironment
  pure
    (proc "sh" ["-e", dataDir </> "boot.sh"])
      { cwd = Just rootBootDir,
        env =
          Just $
            ("ASTERIUS_BOOT_LIBS_DIR", asteriusBootLibsPath)
              : ("ASTERIUS_SANDBOX_GHC_LIBDIR", asteriusSandboxGhcLibDir)
              : ("ASTERIUS_LIB_DIR", bootDir </> "asterius_lib")
              : ("ASTERIUS_TMP_DIR", bootTmpDir args)
              : ("ASTERIUS_AHC", ahc)
              : ("ASTERIUS_AHCPKG", ahcPkg)
              : ("ASTERIUS_SETUP_GHC_PRIM", setupGhcPrim)
              : ("ASTERIUS_CONFIGURE_OPTIONS", configureOptions)
              : [(k, v) | (k, v) <- e, k /= "GHC_PACKAGE_PATH"],
        delegate_ctlc = True
      }

bootRTSCmm :: BootArgs -> IO ()
bootRTSCmm bootArgs@BootArgs {..} =
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut
    $ GHC.runGhc (Just obj_topdir)
    $ do
      let rts_path = asteriusBootLibsPath </> "rts"
      dflags0 <- GHC.getSessionDynFlags
      _ <-
        GHC.setSessionDynFlags $
          GHC.setGeneralFlag' GHC.Opt_SuppressTicks dflags0
      dflags <- GHC.getSessionDynFlags
      is_debug <- isJust <$> liftIO (lookupEnv "ASTERIUS_DEBUG")
      obj_paths_ref <- liftIO $ newIORef []
      cmm_files <-
        liftIO
          $ fmap (filter ((== ".cmm") . takeExtension))
          $ listFilesRecursive
          $ takeDirectory rts_path
      runCmm
        defaultConfig
          { ghcFlags =
              [ "-this-unit-id",
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
                  ( GHC.Module GHC.rtsUnitId $ GHC.mkModuleName $
                      takeBaseName
                        obj_path
                  )
             in runCodeGen (marshalCmmIR ms_mod ir) dflags ms_mod >>= \case
                  Left err -> throwIO err
                  Right m -> do
                    let out_path = bootDir </> makeRelative asteriusBootLibsPath obj_path
                    createDirectoryIfMissing True $ takeDirectory out_path
                    putFile obj_path $ toCachedModule m
                    modifyIORef' obj_paths_ref (out_path :)
                    when is_debug $ do
                      let p = (out_path -<.>)
                      writeFile (p "dump-wasm-ast") $ show m
                      cmm_raw <- Stream.collect cmmRaw
                      writeFile (p "dump-cmm-raw-ast") $ show cmm_raw
                      asmPrint dflags (p "dump-cmm-raw") cmm_raw
        )
      liftIO $ do
        obj_paths <- readIORef obj_paths_ref
        callProcess
          "ar"
          $ ["-r", "-c", obj_topdir </> "rts" </> "libHSrts.a"]
            ++ obj_paths
  where
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
  dataDir <- Paths_asterius.getDataDir
  runBootCreateProcess
    cp_boot
      { cmdspec = RawCommand "sh" ["-e", dataDir </> "boot-init.sh"]
      }
  bootRTSCmm args
  runBootCreateProcess cp_boot
  is_debug <- isJust <$> lookupEnv "ASTERIUS_DEBUG"
  unless is_debug $ removePathForcibly $ bootTmpDir args
