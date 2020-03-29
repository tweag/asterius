{-# LANGUAGE OverloadedStrings #-}

module Asterius.JSRun.Main
  ( newAsteriusInstance,
    hsInit,
    hsMain,
    hsStdOut,
    hsStdErr,
  )
where

import Asterius.BuildInfo
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import Language.JavaScript.Inline.Core
import System.FilePath

newAsteriusInstance :: JSSession -> FilePath -> LBS.ByteString -> IO JSVal
newAsteriusInstance s req_path mod_buf = do
  rts_val <- importMJS s $ dataDir </> "rts" </> "rts.mjs"
  req_mod_val <- importMJS s req_path
  req_val <- eval s $ takeJSVal req_mod_val <> ".default"
  buf_val <- alloc s mod_buf
  mod_val <- eval s $ "WebAssembly.compile(" <> takeJSVal buf_val <> ")"
  eval s $
    takeJSVal rts_val
      <> ".newAsteriusInstance(Object.assign("
      <> takeJSVal req_val
      <> ",{module:"
      <> takeJSVal mod_val
      <> "}))"

hsInit :: JSSession -> JSVal -> IO ()
hsInit s i = eval s $ deRefJSVal i <> ".exports.hs_init()"

hsMain :: String -> JSSession -> JSVal -> IO ()
hsMain prog_name s i =
  eval s $
    deRefJSVal i
      <> ".exports.main().catch(err => { if (!(err.startsWith('ExitSuccess') || err.startsWith('ExitFailure '))) { "
      <> deRefJSVal i
      <> ".fs.writeSync(2, `"
      <> coerce (string7 prog_name)
      <> ": ${err}\n`);}})"

hsStdOut :: JSSession -> JSVal -> IO LBS.ByteString
hsStdOut s i = eval s $ deRefJSVal i <> ".stdio.stdout()"

hsStdErr :: JSSession -> JSVal -> IO LBS.ByteString
hsStdErr s i = eval s $ deRefJSVal i <> ".stdio.stderr()"
