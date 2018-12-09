{-# LANGUAGE OverloadedLists #-}
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
import Data.Foldable
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified DynFlags as GHC
import qualified GHC
import Language.Haskell.GHC.Toolkit.BuildInfo (ahcGccPath, bootLibsPath)
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
  , rtsOnly :: Bool
  }

defaultBootArgs :: BootArgs
defaultBootArgs =
  BootArgs
    { bootDir = dataDir </> ".boot"
    , configureOptions =
        "--disable-shared --disable-profiling --disable-library-for-ghci --disable-split-objs --disable-split-sections -O1 --with-gcc=" <>
        ahcGccPath
    , buildOptions = ""
    , installOptions = ""
    , builtinsOptions = defaultBuiltinsOptions
    , rtsOnly = False
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
  GHC.runGhc (Just ghcLibDir) $
  lowerCodensity $ do
    dflags <- lift GHC.getSessionDynFlags
    setDynFlagsRef dflags
    is_debug <- isJust <$> liftIO (lookupEnv "ASTERIUS_DEBUG")
    store_ref <- liftIO $ newIORef mempty
    rts_cmm_mods <-
      map takeBaseName . filter ((== ".cmm") . takeExtension) <$>
      liftIO (listDirectory rts_path)
    cmms <-
      M.toList <$>
      liftCodensity
        (runCmm
           defaultConfig
             { ghcFlags =
                 [ "-this-unit-id"
                 , "rts"
                 , "-dcmm-lint"
                 , "-O2"
                 , "-pgmc" <> ahcGccPath
                 ]
             }
           [rts_path </> m <.> "cmm" | m <- rts_cmm_mods])
    liftIO $ do
      for_ cmms $ \(fn, ir@CmmIR {..}) ->
        let ms_mod =
              (GHC.Module GHC.rtsUnitId $ GHC.mkModuleName $ takeBaseName fn)
            mod_sym = marshalToModuleSymbol ms_mod
         in case runCodeGen (marshalCmmIR ir) dflags ms_mod of
              Left err -> throwIO err
              Right m -> do
                encodeAsteriusModule obj_topdir mod_sym m
                modifyIORef' store_ref $ registerModule obj_topdir mod_sym m
                when is_debug $ do
                  let p = asteriusModulePath obj_topdir mod_sym
                  writeFile (p "dump-wasm-ast") $ show m
                  writeFile (p "dump-cmm-raw-ast") $ show cmmRaw
                  asmPrint dflags (p "dump-cmm-raw") cmmRaw
                  writeFile (p "dump-cmm-ast") $ show cmm
                  asmPrint dflags (p "dump-cmm") cmm
      if rtsOnly
        then do
          rts_store <- readIORef store_ref
          store <- decodeStore store_path
          encodeStore store_path $ rts_store <> store
        else do
          store <- readIORef store_ref
          encodeStore store_path store
  where
    rts_path = bootLibsPath </> "rts"
    obj_topdir = bootDir </> "asterius_lib"
    store_path = obj_topdir </> "asterius_store"

boot :: BootArgs -> IO ()
boot args = do
  bootRTSCmm args
  unless (rtsOnly args) $ do
    cp' <- bootCreateProcess args
    withCreateProcess cp' $ \_ _ _ ph ->
      finally
        (do ec <- waitForProcess ph
            case ec of
              ExitFailure _ -> fail "boot failure"
              _ -> pure ())
        (do is_debug <- isJust <$> lookupEnv "ASTERIUS_DEBUG"
            unless is_debug $ removeDirectoryRecursive $ bootTmpDir args)
