import Asterius.FixEnv
import System.Directory
import System.Environment.Blank
import System.Process (callProcess)

main :: IO ()
main = do
  fixEnv
  ahc_cabal_root <- getAppUserDataDirectory "ahc-cabal"
  unsetEnv "CABAL_CONFIG"
  setEnv "CABAL_DIR" ahc_cabal_root True
  args <- getArgs
  callProcess "cabal" args
