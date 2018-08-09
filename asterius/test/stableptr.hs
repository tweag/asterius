import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $
    [ "--input"
    , "test/stableptr/stableptr.hs"
    , "--output-link-report"
    , "test/stableptr/stableptr.link.txt"
    , "--run"
    ] <>
    args
