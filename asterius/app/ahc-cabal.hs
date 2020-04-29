import Asterius.BuildInfo
import Control.Monad
import Data.Foldable
import Data.List
import Distribution.Simple.Utils
import Distribution.Verbosity
import System.Directory
import System.Environment.Blank
import System.FilePath
import System.Process (callProcess)

main :: IO ()
main = do
  args0 <- getArgs
  ahc <- getAhc
  ahcPkg <- getAhcPkg
  ahcLd <- getAhcLd
  env <- getEnvironment
  traverse_ unsetEnv
    $ filter (\k -> ("GHC_" `isPrefixOf` k) || "HASKELL_" `isPrefixOf` k)
    $ map fst env
  unsetEnv "CABAL_CONFIG"
  (args1, old_cabal_config_path) <-
    case findIndex ("--config-file" `isPrefixOf`) args0 of
      Just i
        | arg == "--config-file" ->
          pure
            (take i args0 <> drop (i + 2) args0, args0 !! (i + 1))
        | otherwise -> pure (take i args0 <> drop (i + 1) args0, drop 14 arg)
        where
          arg = args0 !! i
      _ -> case lookup "CABAL_CONFIG" env of
        Just v -> pure (args0, v)
        _ -> do
          home <- getHomeDirectory
          let cabal_prefix_path = home </> ".cabal"
              cabal_config_path = cabal_prefix_path </> "config"
          createDirectoryIfMissing True cabal_prefix_path
          cabal_config_exist <- doesFileExist cabal_config_path
          unless cabal_config_exist $
            callProcess
              "cabal"
              ["--config-file", cabal_config_path, "user-config", "init"]
          pure (args0, cabal_config_path)
  tmp <- getTemporaryDirectory
  withTempDirectory silent tmp "ahc-cabal" $ \tmpdir -> do
    let new_cabal_config_path = tmpdir </> "config"
    copyFile old_cabal_config_path new_cabal_config_path
    callProcess
      "cabal"
      [ "--config-file",
        new_cabal_config_path,
        "user-config",
        "update",
        "-a",
        "benchmarks: False",
        "-a",
        "coverage: False",
        "-a",
        "debug-info: False",
        "-a",
        "executable-dynamic: False",
        "-a",
        "executable-stripping: False",
        "-a",
        "library-for-ghci: False",
        "-a",
        "library-stripping: False",
        "-a",
        "profiling: False",
        "-a",
        "relocatable: False",
        "-a",
        "shared: False",
        "-a",
        "split-objs: False",
        "-a",
        "split-sections: False",
        "-a",
        "tests: False",
        "-a",
        "with-compiler: " <> ahc,
        "-a",
        "with-hc-pkg: " <> ahcPkg
      ]
    callProcess "cabal" (["--config-file", new_cabal_config_path] <> args1)
