import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $
    [ "--input"
    , "test/array/array.hs"
    , "--output-link-report"
    , "test/array/array.link.txt"
    , "--force"
    , "--run"
    ] <>
    args
