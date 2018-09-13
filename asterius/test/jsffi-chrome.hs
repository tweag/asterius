import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $
    [ "--input"
    , "test/jsffi-chrome/jsffi-chrome.hs"
    , "--output-link-report"
    , "test/jsffi-chrome/jsffi-chrome.link.txt"
    , "--browser"
    ] <>
    args
