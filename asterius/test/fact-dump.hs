import Asterius.BuildInfo
import System.Directory
import System.FilePath
import UnliftIO.Environment
import UnliftIO.Process

main :: IO ()
main = do
  pwd <- getCurrentDirectory
  let test_path = pwd </> "test" </> "fact-dump"
  setEnv "ASTERIUS_LIB_DIR" test_path
  withCurrentDirectory test_path $ callProcess ahc ["--make", "fact.hs"]
