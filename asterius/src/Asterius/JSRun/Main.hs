{-# LANGUAGE OverloadedStrings #-}

module Asterius.JSRun.Main
  ( newAsteriusInstance,
    hsMain,
    hsStdOut,
    hsStdErr,
  )
where

import Asterius.BuildInfo
import qualified Data.ByteString.Lazy as LBS
import Language.JavaScript.Inline.Core
import System.FilePath

newAsteriusInstance :: Session -> FilePath -> LBS.ByteString -> IO JSVal
newAsteriusInstance s req_path mod_buf = do
  rts_val <- importMJS s $ dataDir </> "rts" </> "rts.mjs"
  req_mod_val <- importMJS s req_path
  req_val <- evalJSVal s Expression $ jsval req_mod_val <> ".default"
  buf_val <- evalJSVal s Expression $ buffer mod_buf
  mod_val <- evalJSVal s Expression $ "WebAssembly.compile(" <> jsval buf_val <> ")"
  evalJSVal s Expression $
    jsval rts_val
      <> ".newAsteriusInstance(Object.assign("
      <> jsval req_val
      <> ",{module:"
      <> jsval mod_val
      <> "}))"

hsMain :: String -> Session -> JSVal -> IO ()
hsMain prog_name s i =
  evalNone s Expression $
    jsval i
      <> ".exports.main().catch(err => { if (!(err.startsWith('ExitSuccess') || err.startsWith('ExitFailure '))) { "
      <> jsval i
      <> ".fs.writeSync(2, `"
      <> code prog_name
      <> ": ${err}\n`);}})"

hsStdOut :: Session -> JSVal -> IO LBS.ByteString
hsStdOut s i = evalBuffer s Expression $ jsval i <> ".stdio.stdout()"

hsStdErr :: Session -> JSVal -> IO LBS.ByteString
hsStdErr s i = evalBuffer s Expression $ jsval i <> ".stdio.stderr()"
