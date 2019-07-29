{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

import Ar
import Control.Concurrent
import Data.Foldable
import Data.Functor
import Data.Maybe
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.System
import Distribution.Types.BuildInfo
import Distribution.Types.GenericPackageDescription
import Distribution.Types.PackageDescription
import Distribution.Verbosity
import System.Directory
import System.FilePath
import System.IO

-- Let the user (or CI system) know that it
-- is not unusual for there to be no output for a
-- long time.
displayTimeTaken :: IO ()
displayTimeTaken = void . forkIO $ do
  let maxMinutes :: Int = 40
      extraMinutes = 5
  forM_ [1 .. maxMinutes] $ \n -> do
    threadDelay 60000000
    putStrLn $ "binaryen has been building for "
      <> show n <> " min (this is not unusual)."
    hFlush stdout
  threadDelay (extraMinutes * 60000000)
  putStrLn $ "binaryen has been building for over "
    <> show (maxMinutes + extraMinutes)
    <> " min (there might be a problem)."
  hFlush stdout

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { hookedPrograms = [simpleProgram "cmake"]
      , confHook =
          \t@(g_pkg_descr, _) c -> do
            displayTimeTaken
            lbi <- confHook simpleUserHooks t c
            absBuildDir <- makeAbsolute $ buildDir lbi
            pwd <- getCurrentDirectory
            let pkg_descr = packageDescription g_pkg_descr
                binaryen_builddir = absBuildDir </> "binaryen"
                binaryen_installdirs =
                  absoluteInstallDirs pkg_descr lbi NoCopyDest
                binaryen_libdir = libdir binaryen_installdirs
                binaryen_bindir = bindir binaryen_installdirs
                run' prog args stdin_s =
                  runProgramInvocation
                    normal
                    (simpleProgramInvocation prog args)
                      {progInvokeInput = Just stdin_s}
                run prog args stdin_s =
                  let conf_prog = fromMaybe (error $ "unable to find " <> show prog) $
                        lookupProgram prog (withPrograms lbi)
                   in runProgramInvocation
                        (fromFlagOrDefault
                           normal
                           (configVerbosity (configFlags lbi)))
                        (programInvocation conf_prog args)
                          {progInvokeInput = Just stdin_s}
            for_ [binaryen_builddir, binaryen_libdir, binaryen_bindir] $
              createDirectoryIfMissing True
            withCurrentDirectory binaryen_builddir $
              for_
                [ [ "-DCMAKE_BUILD_TYPE=Release"
                  , "-DBUILD_STATIC_LIB=ON"
                  , "-G"
                  , "Unix Makefiles"
                  , pwd </> "binaryen"
                  ]
                , ["--build", binaryen_builddir]
                ] $ \args -> run (simpleProgram "cmake") args ""
            binaryen_libs <- listDirectory $ binaryen_builddir </> "lib"
            let output_fn = binaryen_libdir </> "libHSbinaryen-binaryen.a"
                archives =
                  [binaryen_builddir </> "lib" </> l | l <- binaryen_libs]
                (write_ar, is_symdef)
                  | buildOS == OSX = (writeBSDAr, isBSDSymdef)
                  | otherwise = (writeGNUAr, isGNUSymdef)
            ar <- foldlM (\acc p -> (<> acc) <$> loadAr p) mempty archives
            write_ar output_fn $ afilter (not . is_symdef) ar
            run' "ranlib" [output_fn] ""
            binaryen_bins <- listDirectory $ binaryen_builddir </> "bin"
            for_ binaryen_bins $ \b ->
              copyFile
                (binaryen_builddir </> "bin" </> b)
                (binaryen_bindir </> b)
            pure
              lbi
                { localPkgDescr =
                    updatePackageDescription
                      ( Just
                          emptyBuildInfo
                            { extraLibs = ["HSbinaryen-binaryen", "stdc++"]
                            , extraLibDirs = [binaryen_libdir]
                            }
                      , []) $
                    localPkgDescr lbi
                }
      }
