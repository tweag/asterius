{-# LANGUAGE OverloadedStrings #-}

import Asterius.JSRun.Main
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS
import Data.Word
import Language.JavaScript.Inline.Core
import System.Environment
import System.Process

evalWord :: JSSession -> JSCode -> IO Word32
evalWord s expr = do
  buf_val <- eval s "Buffer.allocUnsafe(4)"
  () <- eval s $ deRefJSVal buf_val <> ".writeUInt32LE(" <> expr <> ")"
  buf <- eval s $ takeJSVal buf_val
  pure $ runGet getWord32host buf

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
    x <-
      evalWord s $
      deRefJSVal i <> ".wasmInstance.exports.rts_getInt(" <> deRefJSVal i <>
      ".symbolTable.NoMain_x_closure" <>
      ")"
    print x
