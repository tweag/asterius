{-# LANGUAGE OverloadedStrings #-}

module Asterius.JSRun.Main
  ( newAsteriusInstance,
    hsInit,
    hsMain,
    hsStdOut,
    hsStdErr,
  )
where

import qualified Data.ByteString.Lazy as LBS
import Language.JavaScript.Inline.Core

newAsteriusInstance :: JSSession -> FilePath -> LBS.ByteString -> IO JSVal
newAsteriusInstance s lib_path mod_buf = do
  lib_val <- importMJS s lib_path
  f_val <- eval s $ takeJSVal lib_val <> ".default"
  buf_val <- alloc s mod_buf
  mod_val <- eval s $ "WebAssembly.compile(" <> takeJSVal buf_val <> ")"
  eval s $ takeJSVal f_val <> "(" <> takeJSVal mod_val <> ")"

hsInit :: JSSession -> JSVal -> IO ()
hsInit s i = eval s $ deRefJSVal i <> ".exports.hs_init()"

hsMain :: JSSession -> JSVal -> IO ()
hsMain s i = eval s $ deRefJSVal i <> ".exports.main()"

hsStdOut :: JSSession -> JSVal -> IO LBS.ByteString
hsStdOut s i = eval s $ deRefJSVal i <> ".stdio.stdout()"

hsStdErr :: JSSession -> JSVal -> IO LBS.ByteString
hsStdErr s i = eval s $ deRefJSVal i <> ".stdio.stderr()"
