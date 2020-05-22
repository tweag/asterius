import System.Environment.Blank
import System.Process (callProcess)

main :: IO ()
main = do
  args <- getArgs
  callProcess "ar" args -- For now just call GNU ar

