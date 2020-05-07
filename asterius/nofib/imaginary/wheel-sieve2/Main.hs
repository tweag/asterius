-- Mark II lazy wheel-sieve.
-- Colin Runciman (colin@cs.york.ac.uk); March 1996.
-- See article "Lazy wheel sieves and spirals of primes" (to appear, JFP).

import System.Environment
import Control.Monad (forM_)

prime :: Int -> Int
prime n = primes !! n
  where
    primes = spiral (wheels primes) primes (squares primes) n

spiral (Wheel s ms ns:ws) ps qs input =
  foldr turn0 (roll s) ns
  where
  roll o = foldr (turn o) (foldr (turn o) (roll (o+s)) ns) ms
  turn0  n rs =
    if n<q then n:rs else sp
  turn o n rs =
    let n' = o+n in
    if n'==2 || n'<q then n':rs else dropWhile (<n') sp
  sp = spiral ws (tail ps) (tail qs) input
  -- It's always the case that input^4 > head qs,
  -- but GHC doesn't know that. We do this so that stuff
  -- isn't floated to top-level into a CAF.
  q = min (input^4) (head qs)

squares :: [Int] -> [Int]
squares primes = [p*p | p <- primes]

data Wheel = Wheel Int [Int] [Int]

wheels :: [Int] -> [Wheel]
wheels primes = Wheel 1 [1] [] :
                zipWith3 nextSize (wheels primes) primes (squares primes)

nextSize (Wheel s ms ns) p q =
  Wheel (s*p) ms' ns'
  where
  (xs, ns') = span (<=q) (foldr turn0 (roll (p-1) s) ns)
  ms' = foldr turn0 xs ms
  roll 0 _ = []
  roll t o = foldr (turn o) (foldr (turn o) (roll (t-1) (o+s)) ns) ms
  turn0  n rs =
    if n`mod`p>0 then n:rs else rs
  turn o n rs =
    let n' = o+n in
    if n'`mod`p>0 then n':rs else rs

main = forM_ [1..100] $ const $ do
	[arg] <- getArgs
	print (prime ((read arg) :: Int))

