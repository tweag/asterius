import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $ ["--input-hs", "test/rts/MVar.hs", "--run"] <> args
  callProcess "ahc-link" $ ["--input-hs", "test/rts/FFI.hs", "--run"] <> args
  callProcess "ahc-link" $
    ["--input-hs", "test/rts/ThreadDelay.hs", "--run"]
      <> args
  callProcess "ahc-link" $ ["--input-hs", "test/rts/ForkIO.hs", "--run"] <> args
