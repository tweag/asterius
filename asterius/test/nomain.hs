import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $
    [ "--input-hs"
    , "test/nomain/NoMain.hs"
    , "--ghc-option=-no-hs-main"
    , "--extra-root-symbol=NoMain_x_closure"
    ] <>
    args
