import Asterius.FixEnv
import qualified Asterius.Sysroot as A
import System.Directory
import System.Environment.Blank
import System.Process (callProcess)
import System.FilePath

main :: IO ()
main = do
  fixEnv
  ahc_cabal_root <- getAppUserDataDirectory "ahc-cabal"
  createDirectoryIfMissing True ahc_cabal_root
  let ahc_cabal_config_path = ahc_cabal_root </> "config"
  ahc_cabal_config <- readFile $ A.srcDir </> "asterius" </> "cabal" </> "config"
  writeFile ahc_cabal_config_path ahc_cabal_config
  unsetEnv "CABAL_CONFIG"
  setEnv "CABAL_DIR" ahc_cabal_root True
  args <- getArgs
  callProcess "cabal" args
