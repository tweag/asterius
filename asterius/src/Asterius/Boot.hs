{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Boot
  ( BootArgs
  , bootDir
  , configureOptions
  , buildOptions
  , defaultBootArgs
  , boot
  , clean
  ) where

import Asterius.BuildInfo
import Data.Functor
import Language.Haskell.GHC.Toolkit.BuildInfo (bootLibsPath)
import Language.Haskell.GHC.Toolkit.Run
import System.Exit
import System.FilePath
import UnliftIO
import UnliftIO.Directory
import UnliftIO.Environment
import UnliftIO.Process

data BootArgs = BootArgs
  { bootDir :: FilePath
  , configureOptions, buildOptions :: String
  }

defaultBootArgs :: BootArgs
defaultBootArgs =
  BootArgs
    { bootDir = dataDir </> ".boot"
    , configureOptions = "--disable-split-objs --disable-split-sections -O2"
    , buildOptions = ""
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

bootRTSCmm :: BootArgs -> IO ()
bootRTSCmm BootArgs {..} = do
  rts_cmm_fns <-
    map (rts_path </>) . filter ((== ".cmm") . takeExtension) <$>
    listDirectory rts_path
  void $ runCmm defaultConfig rts_cmm_fns
  pure ()
  where
    rts_path = bootLibsPath </> "rts"

boot :: BootArgs -> IO ()
boot args = do
  bootRTSCmm args
  cp' <- bootCreateProcess args
  withCreateProcess cp' $ \_ _ _ ph -> do
    ec <- waitForProcess ph
    case ec of
      ExitFailure _ -> throwString "boot failure"
      _ -> pure ()

clean :: BootArgs -> IO ()
clean BootArgs {..} = removePathForcibly bootDir
