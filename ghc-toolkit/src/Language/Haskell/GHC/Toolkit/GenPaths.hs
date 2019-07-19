{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Toolkit.GenPaths
  ( GenPathsOptions(..)
  , genPaths
  ) where

import qualified Data.Map as M
import Distribution.ModuleName
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Types.BuildInfo
import Distribution.Types.Library
import Distribution.Types.PackageDescription
import System.Directory
import System.FilePath

newtype GenPathsOptions = GenPathsOptions
  { targetModuleName :: String
  }

genPaths :: GenPathsOptions -> UserHooks -> UserHooks
genPaths GenPathsOptions {..} h =
  h
    { confHook =
        \t f -> do
          lbi@LocalBuildInfo {localPkgDescr = pkg_descr@PackageDescription {library = Just lib@Library {libBuildInfo = lib_bi}}} <-
            confHook h t f
          let [clbi] = componentNameMap lbi M.! CLibName
              mod_path = autogenComponentModulesDir lbi clbi
              mod_name = fromString targetModuleName
              ghc_libdir = compilerProperties (compiler lbi) M.! "LibDir"
          createDirectoryIfMissing True mod_path
          writeFile (mod_path </> targetModuleName <.> "hs") $
            "module " ++
            targetModuleName ++
            " where\n\n" ++
            concat
              [ let Just conf_prog = lookupProgram prog (withPrograms lbi)
                 in prog_name ++
                    " :: FilePath\n" ++
                    prog_name ++ " = " ++ show (programPath conf_prog) ++ "\n\n"
              | (prog_name, prog) <-
                  [("ghc", ghcProgram), ("ghcPkg", ghcPkgProgram)]
              ] ++
            "ghcLibDir :: FilePath\nghcLibDir = " ++
            show ghc_libdir ++
            "\n\n" ++
            concat
              [ k ++
              " :: FilePath\n" ++
              k ++
              " = " ++
              show
                (d $
                 absoluteComponentInstallDirs
                   pkg_descr
                   lbi
                   (componentUnitId clbi)
                   NoCopyDest) ++
              "\n\n"
              | (k, d) <- [("binDir", bindir), ("dataDir", datadir)]
              ]
          pure
            lbi
              { localPkgDescr =
                  pkg_descr
                    { library =
                        Just
                          lib
                            { libBuildInfo =
                                lib_bi
                                  { otherModules =
                                      mod_name : otherModules lib_bi
                                  , autogenModules =
                                      mod_name : autogenModules lib_bi
                                  }
                            }
                    }
              }
    }
