{-# OPTIONS_GHC -Wall -O2 -fforce-recomp -ddump-to-file -ddump-cmm-raw -keep-hc-file -keep-s-file #-}

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

facts :: [Int]
facts = scanl (*) 1 [1 ..]

foreign import ccall unsafe "print_int" print_int :: Int -> IO ()

main :: IO ()
main = print_int $ fact 5
