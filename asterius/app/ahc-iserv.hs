import Control.Exception
import Data.IORef
import GHCi.Message
import GHCi.Signals
import GHCi.Utils
import Lib
import System.Environment.Blank

main :: IO ()
main = do
  arg0:arg1:_ <- getArgs
  inh <- getGhcHandle $ read arg1
  outh <- getGhcHandle $ read arg0
  installSignalHandlers
  lo_ref <- newIORef Nothing
  uninterruptibleMask $
    serv
      True
      pure
      Pipe {pipeRead = inh, pipeWrite = outh, pipeLeftovers = lo_ref}
