{-# LANGUAGE ScopedTypeVariables #-}

import Asterius.Types
import Data.Binary
import qualified Data.ByteString.Char8 as CBS
import Data.IORef
import Foreign.C.Types
import GHC.IO.Device
import GHC.IO.Handle.FD
import GHCi.Message
import System.IO

main :: IO ()
main = do
  read_handle <-
    fdToHandle'
      (fromIntegral read_fd)
      (Just Stream)
      False
      ""
      ReadMode
      True
  write_handle <-
    fdToHandle'
      (fromIntegral write_fd)
      (Just Stream)
      False
      ""
      WriteMode
      True
  hSetBuffering read_handle NoBuffering
  hSetBuffering write_handle NoBuffering
  lo_ref <- newIORef Nothing
  let p = Pipe
        { pipeRead = read_handle,
          pipeWrite = write_handle,
          pipeLeftovers = lo_ref
        }
  (r :: CBS.ByteString) <- readPipe p get
  print r
  writePipe p (put (CBS.pack "ELINA"))

foreign import javascript "Number(process.env.ASTERIUS_NODE_READ_FD)"
  read_fd :: Int

foreign import javascript "Number(process.env.ASTERIUS_NODE_WRITE_FD)"
  write_fd :: Int
