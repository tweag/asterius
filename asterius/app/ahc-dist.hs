import Asterius.Internals
import Asterius.Main
import Prelude hiding (IO)

main :: IO ()
main = do
  task <- getTask
  ld_result <- decodeFile $ inputHS task
  ahcDistMain putStrLn task ld_result
