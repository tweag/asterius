
import Control.Monad (forM_)
import System.Environment

isdivs :: Int  -> Int -> Bool
isdivs n x = mod x n /= 0

the_filter :: [Int] -> [Int]
the_filter (n:ns) = filter (isdivs n) ns

prime :: Int -> Int
prime n = map head (iterate the_filter [2..n*n]) !! n

main = forM_ [1..100] $ const $ do
	[arg] <- getArgs
	print $ prime (read arg)
