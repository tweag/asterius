import Asterius.JSGen.Constants
import Data.ByteString.Builder
import System.IO

main :: IO ()
main = hPutBuilder stdout rtsConstants
