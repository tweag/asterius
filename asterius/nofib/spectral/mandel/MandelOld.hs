
module Mandel where
import PortablePixmap
import Data.Complex
default ()


mandel::Complex Double -> [Complex Double]
mandel c@(cr :+ ci)  = infiniteMandel
           where
		infiniteMandel = c : (map fn infiniteMandel)
		fn::Complex Double -> Complex Double
		fn (r :+ i) = ((r*r) - (i*i) + cr ) :+ ((2*(r*i)) + ci)

whenConverge::[Complex Double] -> Int -> Double -> Int
whenConverge (x:xs) limit radiusSq
   | (converged x radiusSq)  ||
     (limit == 0) 	   = 0				
   |  otherwise            = 1 + (whenConverge xs (limit-1) radiusSq )

converged::Complex Double -> Double -> Bool
converged (x:+y) r =  r <= (x*x) + (y*y)



mandelset::Double -> Double -> Double -> Double ->
	   Integer -> Integer -> Int -> PixMap
mandelset x y x' y' screenX screenY lIMIT
   = render screenX screenY lIMIT results	
     where

	results::[Int]
	results = [colour b a | a <- [1..screenY] , b <- [1..screenX]]

	colour::Integer -> Integer -> Int
	colour  s t = whenConverge (mandel (initial s t)) lIMIT radiusSq

	radiusSq::Double
	radiusSq = max (x'-x) (y'-y)

	initial::Integer -> Integer -> Complex Double
	initial s t = (x + (((coerce s) * (x' - x)) / (fromInteger screenX)) :+
		       y + (((coerce t) * (y' - y)) / (fromInteger screenY)))

	coerce::Integer -> Double
	coerce  s   = encodeFloat (toInteger s) 0



render::Integer -> Integer -> Int -> [Int] -> PixMap
render width height maxColour intensities
   = createPixmap (fromInteger width) (fromInteger height) maxColour
		  (map fn intensities)
     where
	fn::Int -> (Int,Int,Int)
        fn x = let y = (maxColour - x) in (x,y,y)
