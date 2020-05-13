
module GeomNum

where

import Params (renderLeft,renderTop,renderHeight,windowWidth)
import Rationals (Rationals(..),rndNR)

type Numb = Rationals

class (Fractional a) => GeomNumb a where

	rnd :: a->Int
	zerO,positive,negative :: a->Bool

	xCoordInRange :: a->Bool
	yCoordInRange :: a->Bool

instance GeomNumb Rationals where

   	ratio 0 y = if (positive y) then (0,1) else (0,-1)
      ratio x 0 = if (positive x) then (1,0) else (-1,0)
      ratio x y = (signum x*(n:%%1),signum y*(d:%%1))
                      where (n:%%d)= abs x/abs y

rnd2 :: Numb -> Numb
rnd2 a = rnd a :%% 1

grid :: Numb -> Numb
grid (x:%%1) = (div (x+5) 10 * 10) :%% 1
