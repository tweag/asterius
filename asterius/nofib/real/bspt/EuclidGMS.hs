
module EuclidGMS

			Location(..),location, flip_YORK,
			bisect,toBack,section,findVertices,

where
import GeomNum
import Euclid (Point(..),Line,Halfspace(..),Face(..),Faces,Segment,

import Params (renderTop,renderHeight,renderLeft,windowWidth)
import Stdlib (all_YORK,mkset)

type Partition = Face

mkPart :: Region -> Line -> Partition
mkPart region line = Fc (section region line) line

getPart :: Partition -> Line
getPart p = getMyLine p

data Region = Rg [Face]

mkRegion :: [Face] -> Region
mkRegion faces = Rg faces

getRegion :: Region -> [Face]
getRegion (Rg faces) = faces

newRegion :: Region -> Face -> Region
newRegion (Rg faces) face = Rg (face:faces)

data Location = Coincident | Intersects | ToTheRear | ToTheFore deriving (Eq)

location :: Line -> Segment -> Location
location line (p1,p2) = case (locale p1,locale p2) of
				(Coin,Coin) 	-> Coincident
				(Fore,Rear) 	-> Intersects
				(Rear,Fore) 	-> Intersects
				(Rear,_) 	-> ToTheRear
				(_,Rear) 	-> ToTheRear
				(_,_) 		-> ToTheFore
			where
			locale = space line

bisect :: Face -> Line -> (Face,Face)
bisect (Fc (pt1,pt2) line1) line2 =

		where
		face1 = Fc (pt1,pti) line1

		pti = solve line1 line2

flip_YORK :: Face -> Face
flip_YORK (Fc (a,b) l) = Fc (b,a) (invert l)

toBack :: Point -> Line -> Bool
toBack pt line = space line pt /= Fore

inScreen :: Point -> Bool
inScreen (Pt x y) = xCoordInRange x && yCoordInRange y

renderBorder :: Region
renderBorder = mkRegion (mkPolygon [  Pt left top,

               bottom = fromIntegral renderHeight
               left = fromIntegral renderLeft
               right = fromIntegral windowWidth

section :: Region -> Line -> Segment
section region line = f x

findVertices :: Region -> [Point]
findVertices region = [pts | pts <- xs ++ ys, inRegion region pts]
      where
      xs = [x | (x,_) <- segments]
      ys = [y | (_,y) <- segments]
      segments = map getSegment (getRegion region)

inRegion :: Region -> Point -> Bool
inRegion region pt = all_YORK (map (toBack pt.getPart) (getRegion region))
