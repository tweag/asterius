{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent
import Data.Binary
import qualified Data.ByteString.Char8 as CBS
import Data.IORef
import GHC.IO.Handle.FD
import GHCi.Message
import System.Environment.Blank
import System.IO
import System.Process

main :: IO ()
main = do
  (node_read_fd, host_write_fd) <- createPipeFd
  (host_read_fd, node_write_fd) <- createPipeFd
  host_read_handle <- fdToHandle host_read_fd
  host_write_handle <- fdToHandle host_write_fd
  hSetBuffering host_read_handle NoBuffering
  hSetBuffering host_write_handle NoBuffering
  lo_ref <- newIORef Nothing
  let p = Pipe
        { pipeRead = host_read_handle,
          pipeWrite = host_write_handle,
          pipeLeftovers = lo_ref
        }
  setEnv "ASTERIUS_NODE_READ_FD" (show node_read_fd) True
  setEnv "ASTERIUS_NODE_WRITE_FD" (show node_write_fd) True
  args <- getArgs
  writePipe p (put (CBS.pack "ALICE"))
  _ <-
    forkIO
      $ callProcess "ahc-link"
      $ [ "--input-hs",
          "test/nodeio/nodeio.hs",
          "--binaryen",
          "--verbose-err",
          "--run"
        ]
        <> args
  (r :: CBS.ByteString) <- readPipe p get
  print r
