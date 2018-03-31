import Asterius.Boot
import Distribution.System
import System.Directory
import System.FilePath

main :: IO ()
main = do
  tmpdir <- getTemporaryDirectory
  let bootdir = tmpdir </> "asterius" </> ".boot"
  boot
    BootArgs
      { bootDir = bootdir
      , configureOptions =
          (case buildOS of
             Windows -> "--enable-split-objs"
             _ -> "--enable-split-sections") ++
          " --verbose"
      , buildOptions = ""
      , installOptions = ""
      }
