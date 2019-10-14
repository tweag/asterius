import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $
    [ "--input-hs",
      "test/jsffi/jsffi.hs",
      "--input-mjs",
      "test/jsffi/jsffi.mjs",
      "--export-function=mult_hs_int",
      "--export-function=mult_hs_double",
      "--export-function=putchar",
      "--run"
    ]
      <> args
