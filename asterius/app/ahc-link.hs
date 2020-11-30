import Asterius.FixEnv
import Asterius.Main

main :: IO ()
main = do
  fixEnv
  getTask >>= ahcLinkMain
