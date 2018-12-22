import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $
    [ "--input-hs"
    , "test/bigint/bigint.hs"
    , "--input-mjs"
    , "test/bigint/bigint.mjs"
    , "--run"
    ] <>
    args
