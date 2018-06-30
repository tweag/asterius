import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $
    [ "--input"
    , "test/fib/fib.hs"
    , "--output-link-report"
    , "test/fib/fib.link.txt"
    , "--force"
    , "--run"
    ] <>
    args
