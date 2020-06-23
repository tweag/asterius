
main :: IO ()
main = let x = sum [1..3]
  in print x >> print x
