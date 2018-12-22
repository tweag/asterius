import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $
    [ "--input-hs"
    , "test/vault/vault.hs"
    , "--input-mjs"
    , "test/vault/vault.mjs"
    , "--run"
    ] <>
    args
