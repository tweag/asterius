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
import Asterius.CodeGen
import Control.Monad
import qualified Data.ByteString as BS
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Serialize
import qualified GhcPlugins as GHC
import Language.Haskell.GHC.Toolkit.BuildInfo (bootLibsPath)
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.Run
import System.Exit
import System.FilePath
import Text.Show.Pretty (ppShow)
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
  is_debug <- isJust <$> lookupEnv "ASTERIUS_DEBUG"
  rts_cmm_mods <-
    map takeBaseName . filter ((== ".cmm") . takeExtension) <$>
    listDirectory rts_path
  cmms <-
    M.toList <$>
    runCmm defaultConfig [rts_path </> m <.> "cmm" | m <- rts_cmm_mods]
  for_ cmms $ \(fn, ir@CmmIR {..}) ->
    let ms_mod = (GHC.Module GHC.rtsUnitId $ GHC.mkModuleName $ takeBaseName fn)
        mod_sym = marshalToModuleSymbol ms_mod
     in case runCodeGen (marshalCmmIR ir) GHC.unsafeGlobalDynFlags ms_mod of
          Left err -> throwIO err
          Right m -> do
            p <- moduleSymbolPath obj_topdir mod_sym "asterius_o"
            BS.writeFile p $ encode m
            when is_debug $ do
              p_a <- moduleSymbolPath obj_topdir mod_sym "txt"
              writeFile p_a $ ppShow m
              p_c <- moduleSymbolPath obj_topdir mod_sym "dump-cmm-raw-ast"
              writeFile p_c $ ppShow cmmRaw
  where
    rts_path = bootLibsPath </> "rts"
    obj_topdir = bootDir </> "asterius_lib"

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
