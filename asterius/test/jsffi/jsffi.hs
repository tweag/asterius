{-# OPTIONS_GHC -Wall -ddump-to-file -ddump-rn -ddump-foreign -ddump-stg -ddump-cmm-raw -ddump-asm #-}

import Foreign.StablePtr

foreign import javascript "new Date()" current_time :: IO JSRef

foreign import javascript "console.log(${1})" js_print :: JSRef -> IO ()

foreign import javascript "console.log(String.fromCodePoint(${1}))" js_putchar
  :: Char -> IO ()

foreign import javascript "${1} * ${2}" js_mult :: Int -> Int -> Int

foreign import javascript "console.log(${1})" print_int :: Int -> IO ()

foreign import javascript "${1}" js_stableptr_id
  :: StablePtr Int -> IO (StablePtr Int)

foreign export javascript "mult_hs" (*) :: Int -> Int -> Int

main :: IO ()
main = do
  t <- current_time
  js_print t
  js_putchar 'H'
  let x = js_mult 6 7
  print_int x
  x' <- newStablePtr x >>= js_stableptr_id >>= deRefStablePtr
  print_int x'
