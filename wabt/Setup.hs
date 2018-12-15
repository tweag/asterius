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
import System.FilePath

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { hookedPrograms = [simpleProgram "cmake"]
      , confHook =
          \t@(g_pkg_descr, _) c -> do
            lbi <- confHook simpleUserHooks t c
            absBuildDir <- makeAbsolute $ buildDir lbi
            pwd <- getCurrentDirectory
            let pkg_descr = packageDescription g_pkg_descr
                wabt_builddir = absBuildDir </> "wabt"
                wabt_installdirs = absoluteInstallDirs pkg_descr lbi NoCopyDest
                wabt_prefix = takeDirectory $ bindir wabt_installdirs
                run prog args stdin_s =
                  let Just conf_prog = lookupProgram prog (withPrograms lbi)
                   in runProgramInvocation
                        (fromFlagOrDefault
                           normal
                           (configVerbosity (configFlags lbi)))
                        (programInvocation conf_prog args)
                          {progInvokeInput = Just stdin_s}
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
                ] $ \args -> run (simpleProgram "cmake") args ""
            removePathForcibly $ pwd </> "wabt" </> "bin"
            pure lbi
      }
