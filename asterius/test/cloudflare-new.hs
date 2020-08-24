import System.Directory
import System.Environment
import System.FilePath
import System.Process

main :: IO ()
main = do
  args <- getArgs
  withCurrentDirectory "test/cloudflare-new" $ do
    callCommand "npm install"
    callProcess "ahc-link" $
      [ "--bundle",
        "--browser",
        "--input-hs",
        "Worker.hs",
        "--input-mjs",
        "Worker.mjs",
        "--export-function=handleFetch",
        "--no-main"
      ]
        <> args
    createDirectoryIfMissing True "worker"
    copyFile "Worker.wasm" ("worker" </> "module.wasm")
    callCommand "npx wrangler preview --headless"
