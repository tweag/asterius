
module Main(main) where

import ChessSetList (Tile) -- partain:for hbc
import KnightHeuristic
import Queue
import Control.Monad
import System.Environment
import Data.Char

#define fail ioError


main:: IO ()
main=replicateM_ 100 $ getArgs >>= \ss ->
     if (argsOk ss) then
        print (length (printTour ss))
     else
        fail (userError usageString)
     where
        usageString= "\nUsage: knights <board size> <no solutions> \n"
	argsOk ss = (length ss == 2) && (foldr ((&&) . all_digits) True ss)
	all_digits s = foldr ((&&) . isDigit) True s

printTour::[[Char]] -> [Char]
printTour ss
   = pp (take number (depthSearch (root size) grow isFinished))
     where
        [size,number]     = map (strToInt 0) ss
	strToInt y []     = y
	strToInt y (x:xs) = strToInt (10*y+(fromEnum x - fromEnum '0')) xs
	pp []		  = []
	pp ((x,y):xs)     = "\nKnights tour with " ++ (show x)  ++
	   	            " backtracking moves\n" ++ (show y) ++
			    (pp xs)

grow::(Int,ChessSet) -> [(Int,ChessSet)]
grow (x,y) = zip [(x+1),(x+1)..] (descendents y)

isFinished::(Int,ChessSet) -> Bool
isFinished (x,y) = tourFinished y

root::Int -> Queue (Int,ChessSet)
root sze= addAllFront
             (zip [-(sze*sze)+1,-(sze*sze)+1..]
	          (zipWith
		     startTour
		      [(x,y) | x<-[1..sze], y<-[1..sze]]
		     (take (sze*sze) [sze,sze..])))
             createQueue

depthSearch :: (Eq a) => Queue a -> (a -> [a]) -> (a -> Bool) -> Queue a
depthSearch q growFn finFn
   | emptyQueue q           = []
   | finFn (inquireFront q) = (inquireFront q):
			      (depthSearch (removeFront q) growFn finFn)
   | otherwise    	    = (depthSearch
	                         (addAllFront (growFn (inquireFront q))
					      (removeFront q))
	    	                 growFn
	                         finFn)
