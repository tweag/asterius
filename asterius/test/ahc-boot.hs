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
          "--with-ld=" ++
          ("ld.gold" <.> exeExtension) ++
          " --verbose --enable-split-sections --ghc-option=-keep-hc-files --ghc-option=-keep-s-files --ghc-option=-ddump-to-file --ghc-option=-ddump-asm --ghc-option=-ddump-cmm-raw --ghc-option=-ddump-cmm --ghc-option=-ddump-stg --ghc-option=-ddump-simpl"
      , buildOptions = ""
      , installOptions = ""
      }
