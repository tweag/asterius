import Asterius.Types
import qualified Data.ByteString as BS
import Foreign.C.Types
import GHC.IO.Handle.FD
import System.IO

main :: IO ()
main = do
  read_handle <- fdToHandle $ fromIntegral read_fd
  write_handle <- fdToHandle $ fromIntegral write_fd
  hSetBuffering read_handle NoBuffering
  hSetBuffering write_handle NoBuffering
  BS.hGet read_handle 5 >>= print

foreign import javascript "Number(process.env.ASTERIUS_NODE_READ_FD)" read_fd :: Int

foreign import javascript "Number(process.env.ASTERIUS_NODE_WRITE_FD)" write_fd :: Int
