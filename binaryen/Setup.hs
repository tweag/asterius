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
        postConf = \_ _ _ _ -> pure (),
        buildHook = \pkg_descr lbi hooks flags -> do
          buildHook simpleUserHooks pkg_descr lbi hooks flags
          let verbosity = fromFlag (configVerbosity (configFlags lbi))
              binaryen_builddir = binaryenBuildDir lbi
              run prog args =
                let Just conf_prog = lookupProgram prog (withPrograms lbi)
                 in runProgramInvocation
                      verbosity
                      (programInvocation conf_prog args)
          for_
            [ [ "-DCMAKE_BUILD_TYPE=Release",
                "-DBUILD_STATIC_LIB=ON",
                "-G",
                "Unix Makefiles",
                "-S",
                "binaryen",
                "-B",
                binaryen_builddir
              ],
              ["--build", binaryen_builddir]
            ]
            $ \args -> run (simpleProgram "cmake") args,
        copyHook = \pkg_descr lbi hooks flags -> do
          copyHook simpleUserHooks pkg_descr lbi hooks flags
          let binaryen_builddir = binaryenBuildDir lbi
              [clbi] = componentNameCLBIs lbi CLibName
              binaryen_installdirs =
                absoluteComponentInstallDirs
                  pkg_descr
                  lbi
                  (componentUnitId clbi)
                  (fromFlag (copyDest flags))
              binaryen_libdir = libdir binaryen_installdirs
              binaryen_bindir = bindir binaryen_installdirs
          for_ [binaryen_libdir, binaryen_bindir] $ createDirectoryIfMissing True
          copyFile
            (binaryen_builddir </> "lib" </> "libbinaryen.a")
            (binaryen_libdir </> "libbinaryen.a")
          binaryen_bins <- listDirectory $ binaryen_builddir </> "bin"
          for_ binaryen_bins $ \b ->
            copyFile (binaryen_builddir </> "bin" </> b) (binaryen_bindir </> b)
      }

binaryenBuildDir :: LocalBuildInfo -> FilePath
binaryenBuildDir = (</> "binaryen") . buildDir
