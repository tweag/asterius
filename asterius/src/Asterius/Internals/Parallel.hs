module Asterius.Internals.Parallel
  ( parallelFor,
  )
where

parallelFor :: Monoid r => Int -> [a] -> (a -> IO r) -> IO r
parallelFor n xs fn
  | n >= 2 = error "TODO"
  -- If there are not enough resources, fall back to the sequential version.
  | otherwise = mconcat <$> mapM fn xs
