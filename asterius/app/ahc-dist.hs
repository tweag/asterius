import Asterius.Binary.File
import Asterius.Binary.NameCache
import Asterius.Main

main :: IO ()
main = do
  ncu <- newNameCacheUpdater
  task <- getTask
  ld_result <- getFile ncu $ inputHS task
  ahcDistMain putStrLn task ld_result
