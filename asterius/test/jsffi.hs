import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $
    [ "--input"
    , "test/jsffi/jsffi.hs"
    , "--output-link-report"
    , "test/jsffi/jsffi.link.txt"
    , "--run"
    ] <>
    args
