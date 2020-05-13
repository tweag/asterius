
module Main where
import IntLib
import MyRandom
import Prime

main :: IO ()
main = getContents >>= \ cts -> mapM_ putStr (process (lines cts))

process :: [String] -> [String]
process = doInput initState

doInput :: State -> [String] -> [String]
doInput state []     = []
doInput state (l:ls) = doLine l (\state -> doInput state ls) state

doLine :: String -> (State -> [String]) -> State -> [String]
doLine cs cont rs
 = if t then "Probably prime": rest else "Composite": rest
   where n        = readInteger cs
         (t, rs') = multiTest 100 rs n
         rest     = cont rs'

type State = [Int]
initState  = randomInts 111 47
