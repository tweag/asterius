import System.Process

main :: IO ()
main =
  callProcess
    "ahc-link"
    [ "--input"
    , "test/jsffi/jsffi.hs"
    , "--output-link-report"
    , "test/jsffi/jsffi.link.txt"
    , "--force"
    , "--debug"
    , "--run"
    ]
