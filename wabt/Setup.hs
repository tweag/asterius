{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Foldable
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import System.Directory
import System.FilePath

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { hookedPrograms = [simpleProgram "cmake"]
      , confHook =
          \t c -> do
            lbi <- confHook simpleUserHooks t c
            let verbosity = fromFlag $ configVerbosity $ configFlags lbi
                hs_wabt_builddir = buildDir lbi
                hs_wabt_build_bindir = hs_wabt_builddir </> "wabt"
                run prog args cwd =
                  runProgramInvocation
                    verbosity
                    (programInvocation conf_prog args)
                      {progInvokeCwd = Just cwd, progInvokeInput = Nothing}
                  where
                    Just conf_prog =
                      lookupProgram (simpleProgram prog) (withPrograms lbi)
            tmpdir <- getTemporaryDirectory
            withTempDirectory verbosity tmpdir "HSwabt" $ \wabt_buildroot -> do
              copyDirectoryRecursive verbosity "wabt" wabt_buildroot
              let wabt_builddir = wabt_buildroot </> "build"
              createDirectory wabt_builddir
              run
                "cmake"
                [ "-DBUILD_TESTS=OFF"
                , "-DCMAKE_BUILD_TYPE=Release"
                , "-G"
                , "Unix Makefiles"
                , wabt_buildroot
                ]
                wabt_builddir
              run "cmake" ["--build", wabt_builddir] wabt_builddir
              createDirectoryIfMissing True hs_wabt_build_bindir
              bins <- listDirectory $ wabt_buildroot </> "bin"
              for_ bins $ \b ->
                copyFileWithMetadata
                  (wabt_buildroot </> "bin" </> b)
                  (hs_wabt_build_bindir </> b)
            pure lbi
      , copyHook =
          \pkg_descr lbi h flags -> do
            copyHook simpleUserHooks pkg_descr lbi h flags
            let hs_wabt_builddir = buildDir lbi
                hs_wabt_build_bindir = hs_wabt_builddir </> "wabt"
                uid = localUnitId lbi
                copydest = fromFlag (copyDest flags)
                installDirs =
                  absoluteComponentInstallDirs pkg_descr lbi uid copydest
                binDir = bindir installDirs
            createDirectoryIfMissing True binDir
            bins <- listDirectory hs_wabt_build_bindir
            for_ bins $ \b ->
              copyFileWithMetadata (hs_wabt_build_bindir </> b) (binDir </> b)
      }
