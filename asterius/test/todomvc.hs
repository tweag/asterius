import System.Directory
import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $
    ["--browser", "--bundle", "--input-hs", "test/todomvc/todomvc.hs"]
      <> args
