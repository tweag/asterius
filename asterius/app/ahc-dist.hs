import Asterius.Binary.File
import Asterius.Main

main :: IO ()
main = do
  task <- getTask
  ld_result <- getFile $ inputHS task
  ahcDistMain putStrLn task ld_result
