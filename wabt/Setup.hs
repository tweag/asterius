{-# OPTIONS_GHC -Wall -threaded -rtsopts #-}

import Data.Foldable
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Types.LocalBuildInfo
import System.Directory
import System.FilePath

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { hookedPrograms = [simpleProgram "cmake"],
        buildHook = \pkg_descr lbi hooks flags -> do
          buildHook simpleUserHooks pkg_descr lbi hooks flags
          let verbosity = fromFlag (configVerbosity (configFlags lbi))
              wabt_builddir = wabtBuildDir lbi
              run prog args =
                let Just conf_prog = lookupProgram prog (withPrograms lbi)
                 in runProgramInvocation
                      verbosity
                      (programInvocation conf_prog args)
          for_
            [ [ "-DBUILD_TESTS=OFF",
                "-DCMAKE_BUILD_TYPE=Release",
                "-G",
                "Unix Makefiles",
                "-S",
                "wabt",
                "-B",
                wabt_builddir
              ],
              ["--build", wabt_builddir]
            ]
            $ \args -> run (simpleProgram "cmake") args
          wabt_bins <- listDirectory $ "wabt" </> "bin"
          removeDirectoryRecursive $ "wabt" </> "bin"
          createDirectory $ wabt_builddir </> "bin"
          for_ wabt_bins $ \b ->
            renameFile (wabt_builddir </> b) (wabt_builddir </> "bin" </> b),
        copyHook = \pkg_descr lbi hooks flags -> do
          copyHook simpleUserHooks pkg_descr lbi hooks flags
          let wabt_builddir = wabtBuildDir lbi
              [clbi] = componentNameCLBIs lbi CLibName
              wabt_installdirs =
                absoluteComponentInstallDirs
                  pkg_descr
                  lbi
                  (componentUnitId clbi)
                  (fromFlag (copyDest flags))
              wabt_bindir = bindir wabt_installdirs
          createDirectoryIfMissing True wabt_bindir
          wabt_bins <- listDirectory $ wabt_builddir </> "bin"
          for_ wabt_bins $
            \b -> copyFile (wabt_builddir </> "bin" </> b) (wabt_bindir </> b)
      }

wabtBuildDir :: LocalBuildInfo -> FilePath
wabtBuildDir = (</> "wabt") . buildDir
