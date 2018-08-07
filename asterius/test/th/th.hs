{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -ddump-to-file -ddump-stg -ddump-cmm-raw -ddump-asm #-}

import Splices

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

facts :: [Int]
facts = scanl (*) 1 [1 ..]

factStatic :: Int -> Int
factStatic =
  $(genStaticLookupFunction (take 10 (zip [0 ..] (scanl (*) 1 [1 ..]))))

foreign import ccall unsafe "print_i64" print_i64 :: Int -> IO ()

main :: IO ()
main = do
  print_i64 $ fact 5
  print_i64 $ facts !! 5
  print_i64 $ factStatic 5
