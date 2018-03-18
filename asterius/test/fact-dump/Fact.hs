{-# OPTIONS_GHC -Wall -O2 -fforce-recomp -ddump-to-file -ddump-cmm-raw -keep-hc-file -keep-s-file #-}

module Fact where

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

foreign import ccall safe "f2" f2 :: Int -> IO Int
