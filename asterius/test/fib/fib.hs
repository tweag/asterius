{-# LANGUAGE BangPatterns #-}

fib :: Int -> Int
fib n = go 0 1 0
  where
    go !acc0 acc1 i
      | i == n = acc0
      | otherwise = go acc1 (acc0 + acc1) (i + 1)

foreign import ccall unsafe "print_i64" print_i64 :: Int -> IO ()

foreign import ccall unsafe "print_f64" print_f64 :: Double -> IO ()

main :: IO ()
main = do
  print_i64 $ fib 10
  print_f64 $ cos 0.5
  print_f64 $ 2 ** 3
