{-# OPTIONS_GHC -Wall -ddump-to-file -ddump-stg -ddump-cmm-raw -ddump-asm #-}

import Asterius.ByteString
import Asterius.Types
import qualified Data.ByteString.Char8 as CBS
import Data.Char
import Data.Coerce
import System.IO

foreign import javascript "(new Date()).toString() + String.fromCodePoint(0x10000)" js_get_str
  :: IO JSString

foreign import javascript "['asdf', 'zer0']" js_get_arr :: IO JSArray

foreign import javascript "(new Uint8Array([2, 3, 5, 7])).buffer" js_get_buf
  :: IO JSArrayBuffer

foreign import javascript "console.log(new Uint8Array(${1}))" js_print_buf
  :: JSArrayBuffer -> IO ()

foreign import javascript "console.log(${1})" js_print :: JSVal -> IO ()

foreign import javascript "setTimeout(${1},${2},${3})" js_setTimeout
  :: JSFunction -> Int -> JSVal -> IO ()

main :: IO ()
main = do
  print $ CBS.pack "The limits of my language mean the limits of my world."
  hPutStrLn
    stderr
    "The trouble with the world is that the stupid are cocksure and the intelligent are full of doubt."
  js_str <- js_get_str
  let hs_str = fromJSString js_str
  js_print $
    coerce $
    toJSString $ "From Haskell: " <> hs_str <> " " <> show (ord (last hs_str))
  js_arr <- js_get_arr
  js_print $ coerce $ toJSArray $ fromJSArray js_arr
  js_buf <- js_get_buf
  let hs_buf = byteStringFromJSArrayBuffer js_buf
  js_print_buf $ byteStringToJSArrayBuffer hs_buf
  print hs_buf
  cb1 <- makeHaskellCallback1 js_print
  js_setTimeout cb1 4096 (coerce js_str)
