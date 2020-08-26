
-- import Network.Socket -- Well, it's not booted, this one, so we cannot test it :(
import Data.Word

main :: IO ()
main = do
  do let x = 10
     print x
     print (ntohs x)
  do let x = 10
     print x
     print (ntohl x)
  do let x = 10
     print x
     print (htons x)
  do let x = 10
     print x
     print (htonl x)

foreign import ccall safe "ntohs" ntohs :: Word16 -> Word16
foreign import ccall safe "ntohl" ntohl :: Word32 -> Word32
foreign import ccall safe "htons" htons :: Word16 -> Word16
foreign import ccall safe "htonl" htonl :: Word32 -> Word32

