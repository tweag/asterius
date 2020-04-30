import Asterius.Main
import Control.Concurrent

main :: IO ()
main = do
  task <- getTask
  setNumCapabilities (threadPoolSize task)
  ahcLinkMain task
