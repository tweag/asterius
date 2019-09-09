{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

import qualified Data.ByteString as BS
import GHC.IO.Device
import GHC.IO.Handle.FD
import System.Environment.Blank
import System.IO
import System.Posix.Internals
import System.Process

deriving instance Show IODeviceType

main :: IO ()
main = do
  (node_read_fd, host_write_fd) <- createPipeFd
  (host_read_fd, node_write_fd) <- createPipeFd
  host_read_handle <- fdToHandle host_read_fd
  host_write_handle <- fdToHandle host_write_fd
  hSetBuffering host_read_handle NoBuffering
  hSetBuffering host_write_handle NoBuffering
  setEnv "ASTERIUS_NODE_READ_FD" (show node_read_fd) True
  setEnv "ASTERIUS_NODE_WRITE_FD" (show node_write_fd) True
  args <- getArgs
  BS.hPut host_write_handle "ALICE"
  callProcess "ahc-link"
    $ [ "--input-hs",
        "test/nodeio/nodeio.hs",
        "--binaryen",
        "--verbose-err",
        "--run"
        ]
    <> args
