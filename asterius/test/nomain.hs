{-# LANGUAGE OverloadedStrings #-}

import Asterius.Binary.NameCache
import Asterius.JSRun.NonMain
import Asterius.Types
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
  m <- loadObjectRep ncu "test/nomain/NoMain.unlinked.bin"
  bracket
    ( newSession
        defaultConfig
          { nodeExtraArgs = ["--experimental-wasm-return-call"]
          }
    )
    killSession
    $ \s -> do
      i <-
        newAsteriusInstanceNonMain
          s
          "test/nomain/NoMain"
          ["base_AsteriusziTopHandler_runNonIO_closure", "NoMain_x_closure"]
          m
      let x_closure =
            toJS i
              <> ".exports.rts_apply("
              <> toJS i
              <> ".symbolTable.base_AsteriusziTopHandler_runNonIO_closure,"
              <> toJS i
              <> ".symbolTable.NoMain_x_closure)"
          x_tid =
            "await "
              <> toJS i
              <> ".exports.rts_evalIO("
              <> x_closure
              <> ")"
          x_ret = toJS i <> ".exports.getTSOret(" <> x_tid <> ")"
          x_sp = toJS i <> ".exports.rts_getStablePtr(" <> x_ret <> ")"
          x_val' = toJS i <> ".getJSVal(" <> x_sp <> ")"
          x_val = "(async () => " <> x_val' <> ")()"
      x <- eval s x_val
      LBS.putStr x
