{-# LANGUAGE OverloadedStrings #-}

import Asterius.Binary.File
import Asterius.Binary.NameCache
import Asterius.JSRun.NonMain
import Control.Exception
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
      "--no-main",
      "--extra-root-symbol=base_AsteriusziTopHandler_runNonIO_closure",
      "--extra-root-symbol=NoMain_x_closure"
    ]
      <> args
  ncu <- newNameCacheUpdater
  m <- getFile ncu "test/nomain/NoMain.unlinked.bin"
  bracket
    ( newSession
        defaultConfig
          { nodeExtraArgs = ["--experimental-wasm-return-call"],
            nodeExitOnEvalError = True
          }
    )
    closeSession
    $ \s -> do
      i <-
        newAsteriusInstanceNonMain
          s
          "test/nomain/NoMain"
          ["base_AsteriusziTopHandler_runNonIO_closure", "NoMain_x_closure"]
          m
      let x_closure =
            jsval i
              <> ".exports.rts_apply("
              <> jsval i
              <> ".symbolTable.base_AsteriusziTopHandler_runNonIO_closure,"
              <> jsval i
              <> ".symbolTable.NoMain_x_closure)"
          x_tid =
            "await "
              <> jsval i
              <> ".exports.rts_evalIO("
              <> x_closure
              <> ")"
          x_ret = jsval i <> ".exports.getTSOret(" <> x_tid <> ")"
          x_sp = jsval i <> ".exports.rts_getStablePtr(" <> x_ret <> ")"
          x_val' = jsval i <> ".getJSVal(" <> x_sp <> ")"
          x_val = "(async () => " <> x_val' <> ")()"
      x <- evalBuffer s Expression x_val
      LBS.putStr x
