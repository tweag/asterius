import Asterius.BuildInfo
import Asterius.FixEnv
import Data.Foldable
import Data.List
import qualified Paths_asterius
import System.Directory
import System.Environment.Blank
import System.FilePath
import System.Process (callProcess)

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
      <> (dataDir </> ".boot" </> "asterius_lib")
      <> "\nprogram-locations\n  ar-location: "
      <> ahcAr
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
  callProcess "cabal" args
