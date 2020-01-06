import System.Directory
import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  withCurrentDirectory "test/cloudflare" $ callCommand "npm install"
  callProcess "ahc-link" $
    [ "--bundle",
      "--browser",
      "--input-hs",
      "test/cloudflare/cloudflare.hs",
      "--input-mjs",
      "test/cloudflare/cloudflare.mjs",
      "--export-function=handleFetch",
      "--ghc-option=-no-hs-main",
      "--extra-root-symbol=Worker_x_closure"
    ]
      <> args
  withCurrentDirectory "test/cloudflare" $ callProcess "npm" [ "run", "test" ]
