import System.Directory
import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  withCurrentDirectory "test/mealy-chrome" $ callCommand "npm install"
  mealy_driver <- readFile "test/mealy-chrome/mealy-driver.js"
  callProcess "ahc-link" $
    [ "--browser"
    , "--input"
    , "test/mealy-chrome/mealy-chrome.hs"
    , "--output-link-report"
    , "test/mealy-chrome/mealy-chrome.link.txt"
    , "--extra-root-symbol=Main_initModel_closure"
    , "--extra-root-symbol=Main_mealy_closure"
    , "--extra-root-symbol=base_DataziTuple_fst_closure"
    , "--extra-root-symbol=base_DataziTuple_snd_closure"
    , "--asterius-instance-callback=" <> mealy_driver
    ] <>
    args
