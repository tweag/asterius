{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Boot
  ( BootArgs(..)
  , getDefaultBootArgs
  , boot
  ) where

import Asterius.BuildInfo
import Asterius.Builtins
import Asterius.CodeGen
import Asterius.Internals
import Asterius.JSFFI
import Asterius.Store
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified GhcPlugins as GHC
import Language.Haskell.GHC.Toolkit.BuildInfo (bootLibsPath)
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.Run
import Prelude hiding (IO)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import Text.Show.Pretty (ppShow)

data BootArgs = BootArgs
  { bootDir :: FilePath
  , configureOptions, buildOptions :: String
  , builtinsOptions :: BuiltinsOptions
  , rtsOnly :: Bool
  }

getDefaultBootArgs :: IO BootArgs
getDefaultBootArgs = do
  builtins_opts <- getDefaultBuiltinsOptions
  pure
    BootArgs
      { bootDir = dataDir </> ".boot"
      , configureOptions = "--disable-split-objs --disable-split-sections -O2"
      , buildOptions = ""
      , builtinsOptions = builtins_opts
      , rtsOnly = False
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
  store_ref <- newIORef mempty
  rts_cmm_mods <-
    map takeBaseName . filter ((== ".cmm") . takeExtension) <$>
    listDirectory rts_path
  cmms <-
    M.toList <$>
    runCmm
      defaultConfig
        {ghcFlags = ["-this-unit-id", "rts", "-dcmm-lint", "-O2"]}
      [rts_path </> m <.> "cmm" | m <- rts_cmm_mods]
  for_ cmms $ \(fn, ir@CmmIR {..}) ->
    let ms_mod = (GHC.Module GHC.rtsUnitId $ GHC.mkModuleName $ takeBaseName fn)
        mod_sym = marshalToModuleSymbol ms_mod
     in case runCodeGen
               (marshalCmmIR ir)
               (dflags builtinsOptions)
               ms_mod
               emptyFFIMarshalState of
          Left err -> throwIO err
          Right m -> do
            modifyIORef' store_ref $ addModule mod_sym m
            when is_debug $ do
              p_c <- moduleSymbolPath obj_topdir mod_sym "dump-cmm-raw-ast"
              writeFile p_c $ ppShow cmmRaw
              p_s <- moduleSymbolPath obj_topdir mod_sym "txt"
              writeFile p_s $ ppShow m
  if rtsOnly
    then do
      rts_store <- readIORef store_ref
      store <- decodeFile store_path
      encodeFile store_path $ rts_store <> store
    else do
      createDirectoryIfMissing True obj_topdir
      store <- readIORef store_ref
      encodeFile store_path store
  where
    rts_path = bootLibsPath </> "rts"
    obj_topdir = bootDir </> "asterius_lib"
    store_path = obj_topdir </> "asterius_store"

boot :: BootArgs -> IO ()
boot args = do
  bootRTSCmm args
  unless (rtsOnly args) $ do
    cp' <- bootCreateProcess args
    withCreateProcess cp' $ \_ _ _ ph -> do
      ec <- waitForProcess ph
      case ec of
        ExitFailure _ -> fail "boot failure"
        _ -> pure ()
