{-# OPTIONS_GHC -Wall -O2 -fforce-recomp -ddump-to-file -ddump-cmm-raw -keep-hc-file -keep-s-file #-}

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

main :: IO ()
main = do
    putStrLn "Hello fact"
    print $ fact 5
