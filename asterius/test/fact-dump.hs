import Asterius.BuildInfo
import System.Directory
import System.Environment.Blank
import System.FilePath
import System.Process

main :: IO ()
main = do
  pwd <- getCurrentDirectory
  let test_path = pwd </> "test" </> "fact-dump"
  setEnv "ASTERIUS_LIB_DIR" test_path True
  withCurrentDirectory test_path $ callProcess ahc ["--make", "fact.hs"]
