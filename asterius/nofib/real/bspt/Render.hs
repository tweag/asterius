
module Render

		drawBSPT, drawFaces,
		partitionedDraw,prettyPrintBSPT)

where

import BSPT (BSPT(..),Status(..),countLeaves,scanLine,foldBSPT)
import EuclidGMS (Point(..),Line,Face(..),Region,getRegion,

import Stdlib (mapcat,middle,const3)
import Params (renderTop,renderHeight,renderLeft,windowWidth)
import GeomNum
import MGRlib (line,func,movePrintTo,writeVert)

render :: BSPT -> String
render btree = drawFaces (mapcat (scanLine btree) scanLines)
	where scanLines = rules (fromIntegral renderLeft) (fromIntegral windowWidth) (fromIntegral (renderTop+1))

rules :: Numb -> Numb -> Numb -> Faces
rules left right = rules'

drawBSPT :: BSPT -> String
drawBSPT = foldBSPT (const3 []) drawEmbedded

drawFaces :: Faces -> String
drawFaces = mapcat drawSegment.map faceToSection

partitionedDraw :: BSPT -> String
partitionedDraw = foldBSPT temp3 temp
              where

prettyPrintBSPT :: BSPT -> String
prettyPrintBSPT tree = func 4 ++
			 printBSPT  renderLeft windowWidth (renderHeight+40) tree ++
			 func 15

printBSPT :: Int -> Int -> Int -> BSPT -> String
printBSPT le re d (Cell x _ _) | re-le<=20  =  writeVert ((middle le re),d) (map (\x->x:[]) (show x))
printBSPT le re d (Cell x _ _) =  movePrintTo (middle le re) d (show x)
printBSPT le re d tree | re-le<=20 = writeVert (mid,d) (map (\x->x:[]) (show (countLeaves tree)))

printBSPT le re d (BSP part nodeinfo left right) = movePrintTo mid d "@" ++
                                               (printBSPT le mid (d+14) left) ++
				 	 	 (printBSPT mid re (d+14) right)
						  where mid = middle le re
