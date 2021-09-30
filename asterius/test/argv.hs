import System.Directory
import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $ ["--input-hs", "test/argv/argv.hs"] <> args
  withCurrentDirectory "test/argv" $ do
    callProcess
      "node"
      [ "--experimental-modules",
        "--experimental-wasi-unstable-preview1",
        "--experimental-wasm-return-call",
        "--unhandled-rejections=strict",
        "argv.mjs",
        "extra",
        "flags"
      ]
