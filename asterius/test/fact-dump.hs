import System.Process

main :: IO ()
main =
  callProcess
    "ahc-link"
    [ "--input"
    , "test/fact-dump/fact.hs"
    , "--output-link-report"
    , "test/fact-dump/fact.link.txt"
    , "--debug"
    , "--output-ir"
    , "--run"
    ]
