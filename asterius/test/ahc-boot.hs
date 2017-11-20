import Asterius.Boot
import Control.Exception
import System.Directory
import System.FilePath

main :: IO ()
main = do
  tmpdir <- getTemporaryDirectory
  let bootdir = tmpdir </> "asterius" </> ".boot"
  finally
    (boot
       BootArgs
       { bootDir = bootdir
       , configureOptions = ""
       , buildOptions = ""
       , installOptions = ""
       })
    (removePathForcibly bootdir)
