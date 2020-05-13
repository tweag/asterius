
module BSPT

where
import EuclidGMS (	Location(..),Partition,Region,

import GeomNum
import Stdlib (mapcat,const3)
import Libfuns

data BSPT = Cell Status Region Numb |

data Status = In | Out | On deriving (Eq,Show{-was:Text-})

type Partitioning = (Faces,Faces,Faces)

buildBSPT :: Faces -> BSPT
buildBSPT = buildBSPTAux Out renderBorder

buildBSPTAux :: Status -> Region -> Faces -> BSPT
buildBSPTAux status region [] = mkCell status region
buildBSPTAux _ region faces   = par right (seq left (BSP partition (coin,region) left right))
                      where

			right = buildBSPTAux Out (newRegion region (flip_YORK partition)) fore

bsp' :: Partition -> (Faces,Region) -> BSPT -> BSPT -> BSPT
bsp' part (faces,region) (Cell x _ a) (Cell y _ b) | x==y = Cell x region (a+b)
bsp' part nodeInfo left right = BSP part nodeInfo left right

bsp'' :: Partition -> (Faces, Region) -> BSPT -> BSPT -> BSPT
bsp'' part (faces,region) left right

simplify :: Partition -> Region -> BSPT -> BSPT -> BSPT
simplify _ region (Cell _ _ _) (BSP part (faces,_) left right)

simplify _ region (BSP part (faces,_) left right) (Cell _ _ _)

simplify part region tree1 tree2 = bsp' part ([],region) tree1 tree2

mkCell :: Status -> Region -> BSPT
mkCell status region = Cell status region (areaRegion region)

partFaces :: Line -> Faces -> (Faces,Faces,Faces)
partFaces part [] = ([],[],[])
partFaces part (face@(Fc section _):faces)

					Coincident -> (rear,face:coin,fore)
					Intersects -> (rearHalf:rear,coin,foreHalf:fore)
					ToTheRear  -> (face:rear,coin,fore)
					ToTheFore  -> (rear,coin,face:fore))
				where
				(rear,coin,fore) = rest

				(rearHalf,foreHalf) = bisect face part

heuristic :: Faces -> Line
heuristic (Fc _ l:_) = l

updateFaces :: BSPT -> BSPT -> Faces -> Faces
updateFaces left right = mapcat (rubout right).classifyFace left

classifyFace :: BSPT -> Faces -> [(Face,Status)]
classifyFace tree = mapcat (segments tagStatus tree)
			where
			tagStatus x face = [(face,x)]

rubout :: BSPT -> (Face,Status) -> Faces
rubout tree (face,x) = segments (erase x) tree face
	where erase x y face | x==y = []
			      | otherwise = [face]

segments :: (Status->Face->[a]) -> BSPT -> Face -> [a]
segments cellop (Cell status _ _) face = cellop status face
segments cellop (BSP part@(Fc _ p) _ left right) face@(Fc fs _)

			Coincident -> cellop In face
			Intersects -> segments cellop left leftside ++
					segments cellop right rightside
			ToTheRear  -> segments cellop left face
			ToTheFore  -> segments cellop right face
		where
		(leftside,rightside) = bisect face p

scanLine :: BSPT -> Face -> Faces
scanLine = segments filterInside
		where
		filterInside In face = [face]
		filterInside Out _ = []

foldBSPT :: (Status->Region->Numb->a)->(Partition->(Faces,Region)->a->a->a)->BSPT->a
foldBSPT cellop nodeop (Cell x r a) = cellop x r a
foldBSPT cellop nodeop (BSP part nodeinfo left right)
              = nodeop part nodeinfo left' right'
                where

countLeaves :: BSPT -> Int
countLeaves = foldBSPT (const3 1) plus

area :: BSPT -> Numb
area = foldBSPT sumInRegions plus

areaRegion :: Region -> Numb
areaRegion = sum.map triangleArea.triangles.findVertices

triangles :: [Point] -> [[Point]]
triangles [p1,p2] = []
triangles [p1,p2,p3] = [[p1,p2,p3]]
triangles (p1:p2:ps) = if left/=[] && right /=[] then
                              triangles (p1:p2:left) ++ triangles (p1:p2:right)
                       else triangles (p1:ps++[p2])
                             where
                             (left,right) = partPoints (convert p1 p2) ps

partPoints :: Line -> [Point] -> ([Point],[Point])
partPoints eqn [] = ([],[])
partPoints eqn (p:pts) = if toBack p eqn

				where
				(left,right) = partPoints eqn pts

classifyPoint :: Point -> BSPT -> Status
classifyPoint pt = foldBSPT status (deter pt)
                      where
                      status s _ _ = s
