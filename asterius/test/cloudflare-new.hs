import System.Directory
import System.Environment
import System.FilePath
import System.Process

main :: IO ()
main = do
  args <- getArgs
  withCurrentDirectory "test/cloudflare-new" $ do
    callProcess "ahc-link" $
      [ "--browser",
        "--yolo",
        "--gc-threshold=4",
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
    callCommand "wrangler preview --headless"
