import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $
    ["--input-hs", "test/regression60/regression60.hs", "--run"]
      <> args
