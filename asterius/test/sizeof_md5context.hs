import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $
    ["--input-hs", "test/sizeof_md5context/sizeof_md5context.hs", "--run"]
      <> args
