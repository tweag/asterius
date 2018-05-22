import System.Process

main :: IO ()
main =
  callProcess
    "ahc-link"
    ["--input", "test/fact-dump/fact.hs", "--output-ir", "--run"]
