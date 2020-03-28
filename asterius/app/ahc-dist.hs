import Asterius.Internals
import Asterius.Main

main :: IO ()
main = do
  task <- getTask
  ld_result <- decodeFile $ inputHS task
  ahcDistMain putStrLn task ld_result
