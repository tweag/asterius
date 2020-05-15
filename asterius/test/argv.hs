import System.Environment
import System.Directory
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $ ["--input-hs", "test/argv/argv.hs"] <> args
  withCurrentDirectory "test/argv" $ do
    callProcess "node" ["argv.mjs", "extra", "flags"]

