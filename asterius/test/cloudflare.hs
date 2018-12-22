import System.Directory
import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $
    [ "--bundle"
    , "--browser"
    , "--sync"
    , "--input-hs"
    , "test/cloudflare/cloudflare.hs"
    , "--input-mjs"
    , "test/cloudflare/cloudflare.mjs"
    ] <>
    args
