{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Foldable
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Types.GenericPackageDescription
import Distribution.Verbosity
import System.Directory
import System.Environment.Blank
import System.FilePath

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { hookedPrograms = map simpleProgram ["cmake"]
      , confHook =
          \t@(g_pkg_descr, _) c -> do
            lbi <- confHook simpleUserHooks t c
            absBuildDir <- makeAbsolute $ buildDir lbi
            pwd <- getCurrentDirectory
            wabt_extra_bindir <- getEnv "WABT_BINDIR"
            let pkg_descr = packageDescription g_pkg_descr
                wabt_builddir = absBuildDir </> "wabt"
                wabt_installdirs = absoluteInstallDirs pkg_descr lbi NoCopyDest
                wabt_prefix = takeDirectory $ bindir wabt_installdirs
                run prog args =
                  let Just conf_prog =
                        lookupProgram (simpleProgram prog) (withPrograms lbi)
                   in runProgramInvocation
                        (fromFlagOrDefault
                           normal
                           (configVerbosity (configFlags lbi)))
                        (programInvocation conf_prog args)
                          {progInvokeInput = Nothing}
            createDirectoryIfMissing True wabt_builddir
            withCurrentDirectory wabt_builddir $
              for_
                [ [ "-DBUILD_TESTS=OFF"
                  , "-DCMAKE_BUILD_TYPE=Release"
                  , "-DCMAKE_INSTALL_PREFIX=" <> wabt_prefix
                  , "-G"
                  , "Unix Makefiles"
                  , pwd </> "wabt"
                  ]
                , ["--build", wabt_builddir, "--target", "install"]
                ] $ \args -> run "cmake" args
            case wabt_extra_bindir of
              Just p -> do
                wabt_bins <- listDirectory $ pwd </> "wabt" </> "bin"
                createDirectoryIfMissing True p
                for_ wabt_bins $ \b ->
                  copyFile (pwd </> "wabt" </> "bin" </> b) (p </> b)
              _ -> pure ()
            removePathForcibly $ pwd </> "wabt" </> "bin"
            pure lbi
      }
