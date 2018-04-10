import Asterius.Boot
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
          "--disable-split-objs --disable-split-sections -O2 --verbose"
      , buildOptions = ""
      , installOptions = ""
      }
