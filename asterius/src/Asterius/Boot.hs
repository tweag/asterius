{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Boot
  ( BootArgs (..),
    defaultBootArgs,
    boot,
  )
where

import Asterius.Builtins
import Asterius.Internals
import Asterius.Internals.Temp
import qualified Asterius.Sysroot as A
import Control.Monad
import Data.Foldable
import Data.Maybe
import Language.Haskell.GHC.Toolkit.BuildInfo
  ( bootLibsPath,
    sandboxGhcLibDir,
  )
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process

data BootArgs = BootArgs
  { bootDir :: FilePath,
    configureOptions :: String,
    builtinsOptions :: BuiltinsOptions
  }

defaultBootArgs :: BootArgs
defaultBootArgs =
  BootArgs
    { bootDir = undefined </> ".boot",
      configureOptions =
        unwords
          [ "--disable-shared",
            "--disable-profiling",
            "--disable-debug-info",
            "--disable-library-for-ghci",
            "--disable-split-objs",
            "--disable-split-sections",
            "--disable-library-stripping",
            "--enable-relocatable",
            "-O2",
            "--prefix="
              <> A.sysroot,
            "--global",
            "--ipid=$pkg",
            "--with-ghc=ahc",
            "--with-ghc-pkg=ahc-pkg",
            "--hsc2hs-option=--cross-compile",
            "--ghc-option=-v1",
            "--ghc-option=-dsuppress-ticks"
          ],
      builtinsOptions = defaultBuiltinsOptions
    }

bootTmpDir :: BootArgs -> FilePath
bootTmpDir BootArgs {..} = bootDir </> "dist"

bootCreateProcess :: BootArgs -> IO CreateProcess
bootCreateProcess args@BootArgs {..} = do
  e <- getEnvironment
  pure
    (proc "bash" ["-e", "boot.sh"])
      { cwd = undefined,
        env =
          Just $
            kvDedup $
              ("ASTERIUS_BOOT_LIBS_DIR", bootLibsPath) :
              ("ASTERIUS_SANDBOX_GHC_LIBDIR", sandboxGhcLibDir) :
              ("AHC_TMPDIR", bootTmpDir args) :
              ("ASTERIUS_CONFIGURE_OPTIONS", configureOptions) :
                [(k, v) | (k, v) <- e, k /= "GHC_PACKAGE_PATH"],
        delegate_ctlc = True
      }

bootRTSCmm :: BootArgs -> IO ()
bootRTSCmm BootArgs {..} = do
  cmm_files <-
    map (rts_path </>)
      . filter ((== ".cmm") . takeExtension)
      <$> listDirectory rts_path
  withTempDir "ahc-boot" $ \tmpdir -> do
    for_ cmm_files $ \src ->
      callProcess
        "ahc"
        [ "-c",
          "-O2",
          "-dcmm-lint",
          "-I" <> A.sysroot </> "include",
          "-this-unit-id",
          "rts",
          "-o",
          tmpdir </> takeBaseName src <.> "o",
          src
        ]
    callProcess "ar" $
      ["qDS", A.sysroot </> "rts" </> "libHSrts.a"]
        <> [tmpdir </> takeBaseName src <.> "o" | src <- cmm_files]
  where
    rts_path = bootLibsPath </> "rts"

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
