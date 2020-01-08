import Data.Time.Clock.POSIX
import System.CPUTime
import Data.Time.LocalTime

-- | Print current POSIX time and CPU time
main :: IO ()
main = do
  currentTime <- Data.Time.Clock.POSIX.getCurrentTime
  print currentTime
  cpuTime <- System.CPUTime.getCPUTime
  print cpuTime
