{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Asterius.JSRun.Main
  ( newAsteriusInstance,
    hsMain,
    hsStdOut,
    hsStdErr,
  )
where

import Asterius.BuildInfo
import Control.Exception
import qualified Data.ByteString.Lazy as LBS
import Data.String
import Language.JavaScript.Inline.Core
import System.FilePath

newAsteriusInstance :: Session -> FilePath -> LBS.ByteString -> IO JSVal
newAsteriusInstance s req_path mod_buf = do
  rts_val <- importMJS s $ dataDir </> "rts" </> "rts.mjs"
  req_mod_val <- importMJS s req_path
  req_val <- eval @JSVal s $ toJS req_mod_val <> ".default"
  mod_val <- eval @JSVal s $ "WebAssembly.compile(" <> toJS mod_buf <> ")"
  eval s $
    toJS rts_val
      <> ".newAsteriusInstance(Object.assign("
      <> toJS req_val
      <> ",{module:"
      <> toJS mod_val
      <> "}))"

hsMain :: String -> Session -> JSVal -> IO ()
hsMain prog_name s i =
  evaluate
    =<< eval
      s
      ( toJS i
          <> ".exports.main().catch(err => { if (!(err.startsWith('ExitSuccess') || err.startsWith('ExitFailure '))) { "
          <> toJS i
          <> ".fs.writeSync(2, `"
          <> fromString prog_name
          <> ": ${err}\n`);}})"
      )

hsStdOut :: Session -> JSVal -> IO LBS.ByteString
hsStdOut s i = eval s $ toJS i <> ".stdio.stdout()"

hsStdErr :: Session -> JSVal -> IO LBS.ByteString
hsStdErr s i = eval s $ toJS i <> ".stdio.stderr()"
