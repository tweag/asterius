-- Mark I lazy wheel-sieve.
-- Colin Runciman (colin@cs.york.ac.uk); March 1996.
-- See article "Lazy wheel sieves and spirals of primes" (to appear, JFP).

import System.Environment
import Control.Monad (forM_)


data Wheel = Wheel Int [Int]


prime :: Int -> Int
prime n = primes !! n
  where
    primes = sieve (wheels primes) primes (squares primes) n

sieve (Wheel s ns:ws) ps qs input =
  -- It's always the case that input*input > head ps,
  -- but GHC doesn't know that. We do this so that stuff
  -- isn't floated to top-level into a CAF.
  [n' | o <- s:[s*2,s*3..(min (input*input) (head ps-1))*s],
        n <- ns,
        n'<- [n+o], noFactor n']
  ++
  sieve ws (tail ps) (tail qs) input
  where
  noFactor = if s<=2 then const True else notDivBy ps qs

notDivBy (p:ps) (q:qs) n =
  q > n || n `mod` p > 0 && notDivBy ps qs n

squares :: [Int] -> [Int]
squares ps = [p*p | p<-ps]

wheels :: [Int] -> [Wheel]
wheels ps = ws
  where
    ws = Wheel 1 [1] : zipWith nextSize ws ps

nextSize (Wheel s ns) p =
  Wheel (s*p) ns'
  where
  ns' = [n' | o <- [0,s..(p-1)*s],
              n <- ns,
              n' <- [n+o], n'`mod`p > 0]

main = forM_ [1..100] $ const $ do
	[arg] <- getArgs
	print (prime (read arg))
