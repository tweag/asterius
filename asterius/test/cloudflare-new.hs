import System.Directory
import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  withCurrentDirectory "test/cloudflare-new" $ callCommand "npm install"
  callProcess "ahc-link" $
    [ "--bundle",
      "--browser",
      "--input-hs",
      "test/cloudflare-new/Worker.hs",
      "--input-mjs",
      "test/cloudflare-new/Worker.mjs",
      "--export-function=handleFetch",
      "--no-main"
    ]
      <> args
