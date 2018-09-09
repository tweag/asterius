{-# OPTIONS_GHC -Wall -ddump-to-file -ddump-rn -ddump-foreign -ddump-stg -ddump-cmm-raw -ddump-asm #-}

import AsteriusPrim
import Foreign.StablePtr

foreign import javascript "new Date()" current_time :: IO JSRef

foreign import javascript "console.log(${1})" js_print :: JSRef -> IO ()

foreign import javascript "console.log(String.fromCodePoint(${1}))" js_putchar
  :: Char -> IO ()

foreign import javascript "${1} * ${2}" js_mult :: Int -> Int -> Int

foreign import javascript "console.log(${1})" print_int :: Int -> IO ()

foreign import javascript "${1}" js_stableptr_id
  :: StablePtr Int -> IO (StablePtr Int)

foreign import javascript "false" js_false :: Bool

foreign import javascript "true" js_true :: Bool

foreign import javascript "() => __asterius_jsffi.evalStableIO(${1})" js_make_hs_callback
  :: StablePtr (IO ()) -> JSRef

foreign import javascript "setTimeout(${1},${2})" js_set_timeout
  :: JSRef -> Int -> IO ()

foreign export javascript "mult_hs" (*) :: Int -> Int -> Int

main :: IO ()
main = do
  t <- current_time
  js_print t
  js_putchar 'H'
  let x = js_mult 6 7
  print_int x
  x' <- newStablePtr 233 >>= js_stableptr_id >>= deRefStablePtr
  print_int x'
  js_print $
    toJSString $
    fromJSString $ toJSString "I AM A STRING THAT LEAPS BETWEEN HEAPS"
  js_print $ toJSArray $ fromJSArray $ toJSArray [t, t, t]
  js_print $ callJSObjectMethod json "parse" [toJSString "{}"]
  print_int $ fromEnum js_false
  print_int $ fromEnum js_true
  io <- newStablePtr $ print_int 123456
  js_set_timeout (js_make_hs_callback io) 1000
