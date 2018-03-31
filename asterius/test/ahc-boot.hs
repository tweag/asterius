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
          " --verbose --ghc-option=-keep-hc-files --ghc-option=-keep-s-files --ghc-option=-ddump-to-file --ghc-option=-ddump-asm --ghc-option=-ddump-cmm-raw --ghc-option=-ddump-cmm --ghc-option=-ddump-stg --ghc-option=-ddump-simpl"
      , buildOptions = ""
      , installOptions = ""
      }
