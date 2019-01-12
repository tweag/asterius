{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import qualified Data.Map as M
import Distribution.Simple
import Distribution.Simple.BuildPaths hiding (exeExtension)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Types.BuildInfo
import Distribution.Types.Library
import Distribution.Types.PackageDescription
import System.Directory
import System.FilePath
import System.Process

import Data.List (intercalate)

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { confHook =
          \t f -> do
            lbi@LocalBuildInfo { localPkgDescr = pkg_descr@PackageDescription {library = Just lib@Library {libBuildInfo = bi}}
                               , compiler = Compiler {compilerProperties = m}
                               , withPrograms = prog_db
                               } <- confHook simpleUserHooks t f
            let [clbi@LibComponentLocalBuildInfo {componentUnitId = uid}] =
                  componentNameMap lbi M.! CLibName
                amp = autogenComponentModulesDir lbi clbi
                self_installdirs =
                  absoluteComponentInstallDirs pkg_descr lbi uid NoCopyDest
                self_bindir = bindir self_installdirs
                self_datadir = datadir self_installdirs
                rts_datadir = self_datadir </> "boot-libs" </> "rts"
                Just ghc_prog = lookupProgram ghcProgram prog_db
                Just gcc_prog = lookupProgram gccProgram prog_db
            createDirectoryIfMissing True amp
            writeFile (amp </> "BuildInfo_ghc_toolkit.hs") $ intercalate "\n"
              [ "{-# LANGUAGE CPP #-}"
              , "module BuildInfo_ghc_toolkit"
              , "  (getGhcLibDir, getDataDir, getBinDir, getGccPath)"
              , "where"

              , "import qualified Control.Exception as Exception"
              --"import Data.Version (Version(..))"
              , "import System.Environment (getEnv)"
              -- "import Prelude"

              , "#if defined(VERSION_base)"
              , "#if MIN_VERSION_base(4,0,0)"
              , "catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a"
              , "#else"
              , "catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a"
              , "#endif"
              , "#else"
              , "catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a"
              , "#endif"
              , "catchIO = Exception.catch"

              , "ghcLibDir, dataDir, binDir, gccPath :: FilePath"
              , "ghcLibDir = " ++ show (m M.! "LibDir")
              , "dataDir = " ++ show self_datadir
              , "binDir = " ++ show self_bindir
              , "gccPath = " ++ show (locationPath (programLocation gcc_prog))

              , "getGhcLibDir, getDataDir, getBinDir, getGccPath :: IO FilePath"
              , "getGhcLibDir = catchIO (getEnv \"ASTERIUS_GHC_LIB_DIR\")  (\\_ -> return ghcLibDir)"
              , "getDataDir   = catchIO (getEnv \"ASTERIUS_DATA_DIR\")     (\\_ -> return dataDir)"
              , "getBinDir    = catchIO (getEnv \"ASTERIUS_BIN_DIR\")      (\\_ -> return binDir)"
              , "getGccPath   = catchIO (getEnv \"ASTERIUS_GHC_DATA_DIR\") (\\_ -> return gccPath)"
              ]
            autoapply <-
              readProcess
                (replaceBaseName
                   (locationPath (programLocation ghc_prog))
                   "runghc")
                [ "--ghc-arg=-Ighc-libdir/include"
                , "--ghc-arg=-Iinclude-private"
                , "genapply" </> "Main.hs"
                ]
                ""
            createDirectoryIfMissing True rts_datadir
            writeFile (rts_datadir </> "AutoApply.cmm") autoapply
            pure
              lbi
                { localPkgDescr =
                    pkg_descr
                      { library =
                          Just
                            lib
                              { libBuildInfo =
                                  bi
                                    { otherModules =
                                        "BuildInfo_ghc_toolkit" :
                                        otherModules bi
                                    , autogenModules =
                                        "BuildInfo_ghc_toolkit" :
                                        autogenModules bi
                                    }
                              }
                      }
                }
      }
