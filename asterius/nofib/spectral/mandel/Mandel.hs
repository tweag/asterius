
module Mandel where
import Data.Complex -- 1.3
import PortablePixmap
default ()

mandel::(Num a) => a -> [a]
mandel c = infiniteMandel
           where
		infiniteMandel = c : (map (\z -> z*z +c) infiniteMandel)

whenDiverge::  Int -> Double -> Complex Double -> Int
whenDiverge limit radius c
  = walkIt (take limit (mandel c))
  where
     walkIt []     = 0					 -- Converged
     walkIt (x:xs) | diverge x radius  = 0		 -- Diverged
	           | otherwise		 = 1 + walkIt xs -- Keep walking

-- VERY IMPORTANT FUNCTION: sits in inner loop

diverge::Complex Double -> Double -> Bool
diverge cmplx radius =  magnitude cmplx > radius

parallelMandel:: [Complex Double] -> Int -> Double -> [Int]
parallelMandel mat limit radius
   = map (whenDiverge limit radius) mat

mandelset::Double -> 			-- Minimum X viewport
	   Double -> 			-- Minimum Y viewport
	   Double -> 			-- Maximum X viewport
	   Double ->			-- maximum Y viewport
	   Integer -> 			-- Window width
	   Integer -> 			-- Window height
	   Int -> 			-- Window depth
	   PixMap			-- result pixmap
mandelset x y x' y' screenX screenY lIMIT
   = createPixmap screenX screenY lIMIT (map prettyRGB result)
   where

      windowToViewport s t
           = ((x + (((coerce s) * (x' - x)) / (fromInteger screenX))) :+
	      (y + (((coerce t) * (y' - y)) / (fromInteger screenY))))

      coerce::Integer -> Double
      coerce  s   = encodeFloat (toInteger s) 0

      result = parallelMandel
		  [windowToViewport s t | t <- [1..screenY] , s<-[1..screenX]]
 		  lIMIT
		  ((max (x'-x) (y'-y)) / 2.0)

      prettyRGB::Int -> (Int,Int,Int)
      prettyRGB s = let t = (lIMIT - s) in (s,t,t)
