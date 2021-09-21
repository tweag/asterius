import Asterius.FixEnv
import System.Directory
import System.Environment.Blank
import System.Process (callProcess)

hostCabalPath :: IO FilePath
hostCabalPath = do
  ms <- getEnv "AHC_HOST_CABAL"
  maybe (pure "cabal") pure ms

main :: IO ()
main = do
  fixEnv
  ahc_cabal_root <- getAppUserDataDirectory "ahc-cabal"
  unsetEnv "CABAL_CONFIG"
  setEnv "CABAL_DIR" ahc_cabal_root True
  args <- getArgs
  host_cabal <- hostCabalPath
  callProcess host_cabal args
