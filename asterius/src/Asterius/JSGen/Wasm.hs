{-# LANGUAGE OverloadedStrings #-}

module Asterius.JSGen.Wasm
  ( genWasm
  ) where

import Data.ByteString.Builder
import System.FilePath

genWasm :: Bool -> Bool -> FilePath -> Builder
genWasm is_node is_sync base_name =
  mconcat
    [ case () of
        ()
          | is_node -> "import fs from \"fs\";\n"
          | otherwise -> mempty
    , "export default "
    , case () of
        ()
          | is_node && is_sync ->
            "new WebAssembly.Module(fs.readFileSync(" <> out_wasm <> "))"
          | is_node ->
            "fs.promises.readFile(" <> out_wasm <>
            ").then(bufferSource => WebAssembly.compile(bufferSource))"
          | otherwise ->
            "WebAssembly.compileStreaming(fetch(" <> out_wasm <> "))"
    , ";\n"
    ]
  where
    out_wasm = string7 $ show $ base_name <.> "wasm"
