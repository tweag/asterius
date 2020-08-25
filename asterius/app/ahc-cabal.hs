import Asterius.BuildInfo
import Data.Foldable
import Data.List
import qualified Paths_asterius
import System.Directory
import System.Environment.Blank
import System.FilePath
import System.Process (callProcess)

main :: IO ()
main = do
  ahc_cabal_root <- getAppUserDataDirectory "ahc-cabal"
  createDirectoryIfMissing True ahc_cabal_root
  let ahc_cabal_config_path = ahc_cabal_root </> "config"
  ahc_cabal_config <-
    readFile
      =<< Paths_asterius.getDataFileName ("cabal" </> "config")
  writeFile ahc_cabal_config_path $
    "install-dirs user\n  prefix: "
      <> (dataDir </> ".boot" </> "asterius_lib")
      <> "\nprogram-locations\n  ar-location: "
      <> ahcAr
      <> "\n  ghc-location: "
      <> ahc
      <> "\n  ghc-pkg-location: "
      <> ahcPkg
      <> "\n"
      <> ahc_cabal_config
  env <- getEnvironment
  traverse_ unsetEnv $
    filter (\k -> ("GHC_" `isPrefixOf` k) || "HASKELL_" `isPrefixOf` k) $
      map fst env
  unsetEnv "CABAL_CONFIG"
  setEnv "CABAL_DIR" ahc_cabal_root True
  args <- getArgs
  callProcess "cabal" args
