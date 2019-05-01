import Asterius.JSRun.Main
import qualified Data.ByteString.Lazy as LBS
import Language.JavaScript.Inline.Core
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
  mod_buf <- LBS.readFile "test/nomain/NoMain.wasm"
  withJSSession defJSSessionOpts $ \s -> do
    i <- newAsteriusInstance s "test/nomain/NoMain.lib.mjs" mod_buf
    hsInit s i
