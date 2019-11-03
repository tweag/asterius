import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $
    [ "--input-hs",
      "test/rtsapi/rtsapi.hs",
      "--input-mjs",
      "test/rtsapi/rtsapi.mjs",
      "--run",
      "--extra-root-symbol=Main_printInt_closure",
      "--extra-root-symbol=Main_fact_closure",
      "--extra-root-symbol=base_GHCziBase_id_closure",
      "--full-sym-table"
    ]
      <> args
