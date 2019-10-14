{-# OPTIONS_GHC -Wall -ddump-to-file -ddump-rn -ddump-foreign -ddump-stg -ddump-cmm-raw -ddump-asm #-}

import Asterius.Types (JSVal (..))
import AsteriusPrim
import Foreign.StablePtr

foreign import javascript "new Date()" current_time :: IO JSVal

foreign import javascript "console.log(${1})" js_print :: JSVal -> IO ()

foreign import javascript "console.log(String.fromCodePoint(${1}))"
  js_putchar :: Char -> IO ()

foreign import javascript "${1} * ${2}" js_mult :: Int -> Int -> Int

foreign import javascript "console.log(${1})" print_int :: Int -> IO ()

foreign import javascript "${1}"
  js_stableptr_id :: StablePtr Int -> IO (StablePtr Int)

foreign import javascript "false" js_false :: Bool

foreign import javascript "true" js_true :: Bool

foreign import javascript "Math.random()" js_random :: IO Double

foreign import javascript "console.log(${1})" js_print_double :: Double -> IO ()

foreign export javascript "mult_hs_int" (*) :: Int -> Int -> Int

foreign export javascript "mult_hs_double" (*) :: Double -> Double -> Double

foreign export javascript "putchar" js_putchar :: Char -> IO ()

main :: IO ()
main = do
  t <- current_time
  js_print t
  js_putchar 'H'
  let x = js_mult 6 7
  print_int x
  x' <- newStablePtr 233 >>= js_stableptr_id >>= deRefStablePtr
  print_int x'
  js_print $ toJSString $ fromJSString $
    toJSString
      "I AM A STRING THAT LEAPS BETWEEN HEAPS"
  js_print $ toJSArray $ fromJSArray $ toJSArray [t, t, t]
  js_print $ callJSObjectMethod json "parse" [toJSString "{}"]
  print_int $ fromEnum js_false
  print_int $ fromEnum js_true
  js_random >>= js_print_double
  js_random >>= js_print_double
