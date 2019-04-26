{-# LANGUAGE OverloadedStrings #-}

module Asterius.JSRun.Main
  ( jsRun
  ) where

import qualified Data.ByteString.Lazy as LBS
import Language.JavaScript.Inline.Core

jsRunWith :: JSSession -> FilePath -> LBS.ByteString -> IO LBS.ByteString
jsRunWith s lib_path mod_buf = do
  lib_val <- importMJS s lib_path
  f_val <- eval s $ takeJSVal lib_val <> ".default"
  buf_val <- alloc s mod_buf
  mod_val <- eval s $ "WebAssembly.compile(" <> takeJSVal buf_val <> ")"
  i_val <- eval s $ takeJSVal f_val <> "(" <> takeJSVal mod_val <> ")"
  () <- eval s $ deRefJSVal i_val <> ".wasmInstance.exports.hs_init()"
  () <- eval s $ deRefJSVal i_val <> ".wasmInstance.exports.main()"
  eval s $ deRefJSVal i_val <> ".stdio.stdout()"

jsRun :: [String] -> FilePath -> LBS.ByteString -> IO LBS.ByteString
jsRun node_args lib_path mod_buf =
  withJSSession defJSSessionOpts {nodeExtraArgs = node_args} $ \s ->
    jsRunWith s lib_path mod_buf
