
module Main where
import System.Environment
import Control.Monad
import NofibUtils

carryPropagate base (d:ds)
  | carryguess == (d+9) `div` base
      = carryguess : (remainder+nextcarry) : fraction
  | otherwise
      = (dCorrected `div` base) : (dCorrected `mod` base) : fraction
  where carryguess = d `div` base
        remainder = d `mod` base
	  nextcarry:fraction = carryPropagate (base+1) ds
        dCorrected = d + nextcarry

e :: Int -> String
e n =
    take n $
    ("2."++) $
    tail . concat $
    map (show.head) $
    iterate (carryPropagate 2 . map (10*) . tail) $
    take (2*n) $ -- an upper bound on what the pipeline might consume
    2:[1,1..]

main = replicateM_ 100 $ do
	[digits] <- getArgs
	print (hash (show (e (read digits))))
