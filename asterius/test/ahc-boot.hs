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
      , configureOptions = "--ghc-option=-ddump-to-file --ghc-option=-ddump-asm --ghc-option=-ddump-cmm-raw --ghc-option=-ddump-stg --ghc-option=-ddump-simpl --ghc-option=-ddump-splices"
      , buildOptions = ""
      , installOptions = ""
      }
