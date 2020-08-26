import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $ ["--input-hs", "test/endianness/endianness.hs", "--run"] <> args
