{-# LANGUAGE OverloadedStrings #-}

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
    , "--full-sym-table"
    , "--ghc-option=-no-hs-main"
    , "--extra-root-symbol=NoMain_x_closure"
    ] <>
    args
  mod_buf <- LBS.readFile "test/nomain/NoMain.wasm"
  withJSSession defJSSessionOpts $ \s -> do
    i <- newAsteriusInstance s "test/nomain/NoMain.lib.mjs" mod_buf
    hsInit s i
    let x_closure = deRefJSVal i <> ".symbolTable.NoMain_x_closure"
        x_tid =
          deRefJSVal i <> ".wasmInstance.exports.rts_eval(" <> x_closure <> ")"
        x_ret =
          deRefJSVal i <> ".wasmInstance.exports.getTSOret(" <> x_tid <> ")"
        x_sp =
          deRefJSVal i <> ".wasmInstance.exports.rts_getStablePtr(" <> x_ret <>
          ")"
        x_val = deRefJSVal i <> ".getJSVal(" <> x_sp <> ")"
    x <- eval s x_val
    LBS.putStr x
