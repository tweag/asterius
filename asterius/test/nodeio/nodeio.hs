{-# LANGUAGE OverloadedStrings #-}

import Asterius.Types
import qualified Data.ByteString as BS
import Foreign.C.Types
import GHC.IO.Device
import GHC.IO.Handle.FD
import System.IO

main :: IO ()
main = do
  read_handle <-
    fdToHandle' (fromIntegral read_fd)
      (Just Stream)
      False
      ""
      ReadMode
      True
  write_handle <-
    fdToHandle' (fromIntegral write_fd)
      (Just Stream)
      False
      ""
      WriteMode
      True
  hSetBuffering read_handle NoBuffering
  hSetBuffering write_handle NoBuffering
  BS.hGet read_handle 5 >>= print
  BS.hPut write_handle "ELINA"

foreign import javascript "Number(process.env.ASTERIUS_NODE_READ_FD)" read_fd :: Int

foreign import javascript "Number(process.env.ASTERIUS_NODE_WRITE_FD)" write_fd :: Int
