import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $
    [ "--input-hs"
    , "test/bytearray/bytearray.hs"
    , "--input-mjs"
    , "test/bytearray/bytearray.mjs"
    , "--run"
    ] <>
    args
