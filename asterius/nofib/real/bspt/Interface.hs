
module Interface

where

import Init (indicate,labelClassify,labelDefinePoly,unlabelButtons,clearRender,

import Params (Command(..))
import Stdlib (mapcat)
import Euclid (Point(..),mkPolygon,Face,Faces)
import BSPT (BSPT,Status(..),classifyPoint,buildBSPT,area)
import Render (render,drawBSPT,partitionedDraw,drawFaces,prettyPrintBSPT)
import GeomNum
import Merge (union,intersection,subtract_YORK,complement)
import Interpret (Operation,Operations)

modeller :: BSPT -> Operations -> String	
modeller current ((Quit,_):_) = reset
modeller current (operation@(op,_):more) =

perform :: BSPT -> Operation -> ([String],BSPT)
perform current (Partition,_)

perform current (Render,_)

perform current (Classify,points)

perform current (Area,_) =

perform current (Complement,_)

perform current (Polygon,operand)

perform current (Null,_) = ([],current)

perform current (cmd,operand)

	     "\n", clearTree, prettyPrintBSPT btree,

boolOp :: Command -> BSPT -> BSPT -> BSPT
boolOp Union current operand = union current operand
boolOp Intersect current operand = intersection current operand
boolOp Subtract current operand = subtract_YORK current operand

validate :: [a] -> [a]
validate pts = if (length pts<3) then [] else pts

transform :: [Point] -> [Point]
transform = map trans
