
module Merge

where

import BSPT (BSPT(..),Status(..),bsp',bsp'',mkCell,partFaces,foldBSPT)
import EuclidGMS (Point,Line,Face(..),Region,Location(..),Partition,Faces,

import Stdlib (mappair)
import GeomNum
import Libfuns

-- -------- Type decls ------------------------

union :: BSPT -> BSPT -> BSPT
union = merge rules

		rules :: BSPT -> BSPT -> BSPT						
		rules cell@(Cell In _ _) tree = cell
		rules cell@(Cell Out _ _) tree = tree
		rules tree cell@(Cell In _ _) = cell
		rules tree cell@(Cell Out _ _) = tree

intersection :: BSPT -> BSPT -> BSPT
intersection = merge rules

		rules :: BSPT -> BSPT -> BSPT						
		rules cell@(Cell In _ _) tree = tree
		rules cell@(Cell Out _ _) tree = cell
		rules tree cell@(Cell In _ _) = tree
		rules tree cell@(Cell Out _ _) = cell

subtract_YORK :: BSPT -> BSPT -> BSPT
subtract_YORK x y = intersection x (complement y)

merge :: (BSPT -> BSPT -> BSPT) -> BSPT -> BSPT -> BSPT
merge op (Cell x r a) tree = op (Cell x r a) tree
merge op tree (Cell x r a) = op tree (Cell x r a)
merge op (BSP p nodeinfo left right) tree
				= bsp'' p nodeinfo left' right'
					where	
					left'= merge op left rear
					right'= merge op right fore
					(rear,fore) = partTree p tree

partTree :: Partition -> BSPT -> (BSPT,BSPT)
partTree part (Cell x region a) = (mkCell x (newRegion region part),

partTree part@(Fc sp p) tree@(BSP (Fc st t) (_,region) _ _)

                        (Coincident,_)        -> if p==t
                                                 then onParallel part tree
                                                 else onAntiparallel part tree
                        (ToTheFore,ToTheFore) -> pinPostinPos part tree
                        (ToTheFore,ToTheRear) -> pinNegtinPos part tree
                        (ToTheRear,ToTheFore) -> pinPostinNeg part tree
                        (ToTheRear,ToTheRear) -> pinNegtinNeg part tree
                        (_,_)                 -> inBoth part tree

onParallel :: Partition -> BSPT -> (BSPT,BSPT)
onParallel p (BSP t _ rear fore) = (rear,fore)

onAntiparallel :: Partition -> BSPT -> (BSPT,BSPT)
onAntiparallel p (BSP t _ rear fore) = (fore,rear)

pinPostinNeg :: Partition -> BSPT -> (BSPT,BSPT)
pinPostinNeg p (BSP t (faces,region) tRear tFore)
			= (bsp' t (faces,newRegion region p) tRear tForepRear,

		 	  where
			  (tForepRear,tForepFore) = partTree p tFore

pinPostinPos :: Partition -> BSPT -> (BSPT,BSPT)
pinPostinPos p (BSP t (faces,region) tRear tFore)
			= (tForepRear,

			  where
			  (tForepRear,tForepFore) = partTree p tFore

pinNegtinPos :: Partition -> BSPT -> (BSPT,BSPT)
pinNegtinPos p (BSP t (faces,region) tRear tFore)
			= (tRearpRear,

			  where
			  (tRearpRear,tRearpFore) = partTree p tRear

pinNegtinNeg :: Partition -> BSPT -> (BSPT,BSPT)
pinNegtinNeg p (BSP t (faces,region) tRear tFore)
			= (bsp' t (faces,newRegion region p) tRearpRear tFore,

				where
				(tRearpRear,tRearpFore) = partTree p tRear

inBoth :: Partition -> BSPT -> (BSPT,BSPT)
inBoth p (BSP t (faces,region) tRear tFore)
		= (bsp' tLeft (rearFaces,leftRegion) tRearpRear tForepRear,
		   bsp' tRight (foreFaces,rightRegion) tRearpFore tForepFore)
		where
		(tRearpRear,tRearpFore) = partTree pLeft tRear
		(tForepRear,tForepFore) = partTree pRight tFore
		(rearFaces,_,foreFaces) = partFaces p' faces

complement :: BSPT -> BSPT
complement = foldBSPT compCell BSP
