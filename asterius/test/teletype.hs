import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $
    [ "--input-hs"
    , "test/teletype/teletype.hs"
    , "--input-mjs"
    , "test/teletype/teletype.mjs"
    , "--run"
    ] <>
    args
