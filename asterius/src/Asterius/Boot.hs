{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Boot
  ( BootArgs(..)
  , boot
  ) where

import Asterius.BuildInfo
import Asterius.CodeGen
import Asterius.Types
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Serialize
import Data.String
import Data.Traversable
import DynFlags
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

bootRTSCmm :: BootArgs -> IO ()
bootRTSCmm BootArgs {..} = do
  rts_cmm_fns <-
    map (rts_path </>) . filter ((== ".cmm") . takeExtension) <$>
    listDirectory rts_path
  rts_cmms <- runCmm defaultConfig rts_cmm_fns
  is_debug <- isJust <$> lookupEnv "ASTERIUS_DEBUG"
  rts_sym_db <-
    fmap mconcat $
    for (M.toList rts_cmms) $ \(k, cmm_ir@CmmIR {..}) -> do
      m <- marshalCmmIR unsafeGlobalDynFlags cmm_ir
      let mod_sym =
            AsteriusModuleSymbol
              {unitId = "rts", moduleName = [fromString $ takeBaseName k]}
      p <- moduleSymbolPath obj_topdir mod_sym "asterius_o"
      BS.writeFile p $ encode m
      when is_debug $ do
        p_a <- moduleSymbolPath obj_topdir mod_sym "txt"
        writeFile p_a $ ppShow m
        p_c <- moduleSymbolPath obj_topdir mod_sym "dump-cmm-raw-ast"
        writeFile p_c $ ppShow cmmRaw
      pure $ moduleSymbolDB mod_sym m
  BS.writeFile (obj_topdir </> ".asterius_sym_db") $ encode rts_sym_db
  where
    obj_topdir = bootDir </> "asterius_lib"
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
