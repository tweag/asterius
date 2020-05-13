
module Euclid

		Face(..),mkFace,getSegment,getMyLine,

		mkPoint,mkPolygon,drawSegment)

where
import Stdlib (map2,splitAt_YORK,pair,between,numval)
import GeomNum
import MGRlib (line)
import Params (mouseDispx,mouseDispy,gap)
import Data.Char(isDigit)--1.3

data Line = Ln Numb Numb Numb deriving (Show{-was:Text-},Eq)

data Point = Pt Numb Numb deriving (Eq,Show{-was:Text-})

data Halfspace = Fore | Coin | Rear deriving (Eq,Show{-was:Text-})

data Face = Fc Segment Line deriving (Eq,Show{-was:Text-})

type Segment = (Point,Point)

type Faces = [Face]

mkFace :: Segment -> Face
mkFace (x,y) = Fc (x,y) (convert x y)

getSegment :: Face -> Segment
getSegment (Fc segment _) = segment

getMyLine :: Face -> Line
getMyLine (Fc _ line) = line

space :: Line -> Point -> Halfspace
space line pt = if zerO val then Coin else

eqn :: Line -> Point -> Numb
eqn (Ln a b c) (Pt x y) = a*x + b*y + c

convert :: Point -> Point -> Line
convert (Pt x1 y1) (Pt x2 y2) = Ln (diffy) (-diffx) (diffx*y1-diffy*x1)
                                where

                                      dx=x2-x1
                                      (diffx,diffy) = ratio dx dy

invert :: Line -> Line
invert (Ln a b c) = (Ln (negate a) (negate b) (negate c))

solve :: Line -> Line -> Point
solve (Ln a b c) (Ln d e f) | zerO ((a*e)-(b*d)) = (Pt 0 0)
			    | not (zerO a) 	= solveAux (Ln a b (-c)) (Ln d e (-f))
			    | otherwise 	= solveAux (Ln d e (-f)) (Ln a b (-c))

solveAux :: Line -> Line -> Point
solveAux (Ln a b c) (Ln 0 e f) = (Pt x y)
					where y = f/e
					      x = (c-b*y)/a

solveAux (Ln a b c) (Ln d e f) = solveAux (Ln a b c) (Ln 0 (e-b*g) (f-c*g))

triangleArea :: [Point] -> Numb
triangleArea [p1,p2,p3] = abs ((1/2) * (x1*y2-y1*x2))

minusPoint :: Point -> Point -> Point
minusPoint (Pt x y) (Pt u v) = Pt (u-x) (v-y)

mkPoint :: String -> Point
mkPoint l = if and (map isDigit (a++b)) then
                 Pt (fromIntegral (numval a-mouseDispx))
				(fromIntegral (numval b-mouseDispy))
                else (Pt 0 0) -- Null Point
                                        where
                                        (a,b)= splitAt_YORK gap l

mkPolygon :: [Point] -> Faces
mkPolygon [] = []
mkPolygon (a:l) = map2 f (a:l) (l++[a])

drawSegment :: Segment -> String
drawSegment ((Pt x1 y1),(Pt x2 y2)) = line (map rnd [x1,y1,x2,y2])

{- UNUSED: (an illegal name, too)
inRange :: [Integer] -> Point -> Bool
inRange [x,y,w,h] (Pt a b) = between (fromInteger x) (fromInteger w) a
                                        && between (fromInteger y) (fromInteger h) b
-}
