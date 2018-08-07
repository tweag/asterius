import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $
    [ "--input"
    , "test/th/th.hs"
    , "--output-link-report"
    , "test/th/th.link.txt"
    , "--run"
    ] <>
    args
