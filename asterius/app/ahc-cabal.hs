import Asterius.BuildInfo
import Asterius.FixEnv
import qualified Paths_asterius
import System.Directory
import System.Environment.Blank
import System.FilePath
import System.Process (callProcess)

hostCabalPath :: IO FilePath
hostCabalPath = do
  ms <- getEnv "AHC_HOST_CABAL"
  maybe (pure "cabal") pure ms

main :: IO ()
main = do
  fixEnv
  ahc_cabal_root <- getAppUserDataDirectory "ahc-cabal"
  createDirectoryIfMissing True ahc_cabal_root
  let ahc_cabal_config_path = ahc_cabal_root </> "config"
  ahc_cabal_config <-
    readFile
      =<< Paths_asterius.getDataFileName ("cabal" </> "config")
  writeFile ahc_cabal_config_path $
    "install-dirs global\n  prefix: "
      <> ahcLibDir
      <> "\nprogram-default-options\n  hsc2hs-options: --cross-compile"
      <> "\nwith-compiler: "
      <> ahc
      <> "\nwith-hc-pkg: "
      <> ahcPkg
      <> "\n"
      <> ahc_cabal_config
  unsetEnv "CABAL_CONFIG"
  setEnv "CABAL_DIR" ahc_cabal_root True
  args <- getArgs
  host_cabal <- hostCabalPath
  callProcess host_cabal args
