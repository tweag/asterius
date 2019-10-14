{-# OPTIONS_GHC -Wall -ddump-to-file -ddump-stg -ddump-cmm-raw -ddump-asm #-}

module Main
  ( printInt,
    fact,
    main,
  )
where

foreign import javascript "console.log(${1})" js_print_int :: Int -> IO ()

printInt :: Int -> IO ()
printInt = js_print_int

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

main :: IO ()
main = js_print_int $ fact 5
