module Asterius.JSGen.Wizer where

import Asterius.Internals.Temp
import qualified Data.ByteString as BS
import Data.List
import Data.Maybe
import Data.Word
import Numeric
import System.Environment.Blank
import System.FilePath
import System.IO.Unsafe
import System.Process

wizer' :: FilePath -> Word32 -> IO (BS.ByteString, Word32)
wizer' ahc_rts init_alloc_len = withTempDir "ahc-wizer" $ \d -> do
  init_stdout <-
    readProcess
      "wizer"
      ["--allow-wasi", "-o", d </> "result.wasm", ahc_rts]
      (show init_alloc_len)
  init_addr <-
    case ( do
             s <- stripPrefix "0x" init_stdout
             (r, _) <- listToMaybe $ readHex s
             pure r
         ) of
      Just r -> pure r
      _ -> fail $ "wizer returned " <> init_stdout
  result_wasm_buf <- BS.readFile $ d </> "result.wasm"
  pure (result_wasm_buf, init_addr)

wizer :: Word32 -> IO (BS.ByteString, Word32)
wizer init_alloc_len = do
  Just ahc_rts <- getEnv "AHC_RTS"
  wizer' ahc_rts init_alloc_len

wizerInitAddr :: Word32 -> Word32
wizerInitAddr init_alloc_len = unsafePerformIO $ do
  (_, init_addr) <- wizer init_alloc_len
  pure init_addr
