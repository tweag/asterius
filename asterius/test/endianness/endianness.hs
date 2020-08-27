
-- import Network.Socket -- Well, it's not booted, this one, so we cannot test it :(
import Data.Word

main :: IO ()
main = do
  do let x = 0xABCD
     let y = 0xCDAB
     putStr "ntohs: " >> print (ntohs x == y && x == ntohs y)
  do let x = 0x56789ABC
     let y = 0xBC9A7856
     putStr "ntohl: " >> print (ntohl x == y && x == ntohl y)
  do let x = 0xABCD
     let y = 0xCDAB
     putStr "htons: " >> print (htons x == y && x == htons y)
  do let x = 0x56789ABC
     let y = 0xBC9A7856
     putStr "htonl: " >> print (htonl x == y && x == htonl y)
  --
  do let x = 0xABCD
     let y = 0xCDAB
     putStr "hs_bswap16: " >> print (hs_bswap16 x == y && x == hs_bswap16 y)
  do let x = 0x56789ABC
     let y = 0xBC9A7856
     putStr "hs_bswap32: " >> print (hs_bswap32 x == y && x == hs_bswap32 y)

  do let x = 0x0123456789ABCDEF
     let y = 0xEFCDAB8967452301
     putStr "hs_bswap64: " >> print (hs_bswap64 x == y && x == hs_bswap64 y)

foreign import ccall safe "ntohs" ntohs :: Word16 -> Word16
foreign import ccall safe "ntohl" ntohl :: Word32 -> Word32
foreign import ccall safe "htons" htons :: Word16 -> Word16
foreign import ccall safe "htonl" htonl :: Word32 -> Word32

foreign import ccall safe "hs_bswap16" hs_bswap16 :: Word16 -> Word16
foreign import ccall safe "hs_bswap32" hs_bswap32 :: Word32 -> Word32
foreign import ccall safe "hs_bswap64" hs_bswap64 :: Word64 -> Word64

