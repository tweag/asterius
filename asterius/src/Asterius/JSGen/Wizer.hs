module Asterius.JSGen.Wizer where

import Asterius.Internals.Temp
import qualified Data.ByteString as BS
import Data.List
import Data.Maybe
import Data.Word
import Numeric
import System.FilePath
import System.Process

wizer :: FilePath -> Word64 -> IO (BS.ByteString, Word64)
wizer ahc_rts init_alloc_len = withTempDir "ahc-wizer" $ \d -> do
  init_stdout <-
    readProcess
      "wizer"
      ["--allow-wasi", "-o", d </> "result.wasm", ahc_rts]
      (show init_alloc_len)
  init_alloc_addr <-
    case ( do
             s <- stripPrefix "0x" init_stdout
             (r, _) <- listToMaybe $ readHex s
             pure r
         ) of
      Just r -> pure r
      _ -> fail $ "wizer returned " <> init_stdout
  result_wasm_buf <- BS.readFile $ d </> "result.wasm"
  pure (result_wasm_buf, init_alloc_addr)
