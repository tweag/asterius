{-# OPTIONS_GHC -Wall -ddump-to-file -ddump-rn -ddump-stg -ddump-cmm-raw -ddump-asm #-}

foreign import javascript "new Date()" current_time :: IO JSRef

foreign import javascript "console.log(${1})" js_print :: JSRef -> IO ()

foreign import javascript "console.log(String.fromCodePoint(${1}))" js_putchar :: Char -> IO ()

foreign import javascript "${1} * ${2}" js_mult :: Int -> Int -> Int

foreign import javascript "console.log(${1})" print_int :: Int -> IO ()

main :: IO ()
main = do
  t <- current_time
  js_print t
  js_putchar 'H'
  print_int $ js_mult 6 7
