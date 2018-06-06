import System.Process

main :: IO ()
main =
  callProcess
    "ahc-link"
    [ "--input"
    , "test/fib/fib.hs"
    , "--output-link-report"
    , "test/fib/fib.link.txt"
    , "--force"
    , "--override-store"
    , "--debug"
    , "--run"
    ]
