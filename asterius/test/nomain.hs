{-# LANGUAGE OverloadedStrings #-}

import Asterius.JSRun.Main
import Asterius.JSRun.NonMain
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Language.JavaScript.Inline.Core
import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $
    [ "--input-hs",
      "test/nomain/NoMain.hs",
      "--output-ir",
      "--full-sym-table",
      "--ghc-option=-no-hs-main",
      "--extra-root-symbol=NoMain_x_closure",
      "--no-gc-sections"
    ]
      <> args
  m <- decodeFile "test/nomain/NoMain.unlinked.bin"
  withJSSession defJSSessionOpts {nodeStdErrInherit = True} $ \s -> do
    i <-
      newAsteriusInstanceNonMain
        s
        "test/nomain/NoMain"
        ["NoMain_x_closure"]
        m
    hsInit s i
    let x_closure = deRefJSVal i <> ".symbolTable.NoMain_x_closure"
        x_tid =
          "await " <> deRefJSVal i <> ".exports.rts_eval(" <> x_closure <> ")"
        x_ret = deRefJSVal i <> ".exports.getTSOret(" <> x_tid <> ")"
        x_sp = deRefJSVal i <> ".exports.rts_getStablePtr(" <> x_ret <> ")"
        x_val' = deRefJSVal i <> ".getJSVal(" <> x_sp <> ")"
        x_val = "(async () => " <> x_val' <> ")()"
    x <- eval s x_val
    LBS.putStr x
