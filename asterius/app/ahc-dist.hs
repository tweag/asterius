import Asterius.Binary.File
import Asterius.Binary.NameCache
import Asterius.FixEnv
import Asterius.Main

main :: IO ()
main = do
  fixEnv
  ncu <- newNameCacheUpdater
  task <- getTask
  ld_result <- getFile ncu $ inputHS task
  ahcDistMain putStrLn task ld_result
