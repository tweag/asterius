{-# OPTIONS_GHC -Wall -O2 -ddump-to-file -ddump-rn -ddump-stg -ddump-cmm-raw -ddump-asm #-}

foreign import javascript "new Date()" current_time :: IO JSRef

foreign import javascript "console.log(${1})" js_print :: JSRef -> IO ()

main :: IO ()
main = do
  t <- current_time
  js_print t
