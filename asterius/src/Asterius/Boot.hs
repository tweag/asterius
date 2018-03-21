{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Boot
  ( BootArgs(..)
  , bootQuiet
  , boot
  ) where

import Asterius.BuildInfo
import Language.Haskell.GHC.Toolkit.BuildInfo (bootLibsPath)
import System.Exit
import System.FilePath
import UnliftIO
import UnliftIO.Environment
import UnliftIO.Process

data BootArgs = BootArgs
  { bootDir :: FilePath
  , configureOptions, buildOptions, installOptions :: String
  }

bootTmpDir :: BootArgs -> FilePath
bootTmpDir BootArgs {..} = bootDir </> "dist"

bootCreateProcess :: BootArgs -> IO CreateProcess
bootCreateProcess args@BootArgs {..} = do
  e <- getEnvironment
  pure
    (proc sh ["-e", "boot.sh"])
      { cwd = Just dataDir
      , env =
          Just $
          ("ASTERIUS_BOOT_LIBS_DIR", bootLibsPath) :
          ("ASTERIUS_LIB_DIR", bootDir </> "asterius_lib") :
          ("ASTERIUS_TMP_DIR", bootTmpDir args) :
          ("ASTERIUS_GHC_PATH", ghc) :
          ("ASTERIUS_AHC_PATH", ahc) :
          ("ASTERIUS_MKDIR_PATH", mkdir) :
          ("ASTERIUS_CP_PATH", cp) :
          ("ASTERIUS_CONFIGURE_OPTIONS", configureOptions) :
          ("ASTERIUS_BUILD_OPTIONS", buildOptions) :
          [(k, v) | (k, v) <- e, k /= "GHC_PACKAGE_PATH"]
      , delegate_ctlc = True
      }

bootQuiet :: BootArgs -> IO (ExitCode, String, String)
bootQuiet args = do
  cp' <- bootCreateProcess args
  readCreateProcessWithExitCode cp' ""

boot :: BootArgs -> IO ()
boot args = do
  cp' <- bootCreateProcess args
  withCreateProcess cp' $ \_ _ _ ph -> do
    ec <- waitForProcess ph
    case ec of
      ExitFailure _ -> throwString "boot failure"
      _ -> pure ()
