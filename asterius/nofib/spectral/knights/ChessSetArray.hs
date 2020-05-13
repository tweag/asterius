
module ChessSetArray(Tile,
		    ChessSet,
		    createBoard,
		    sizeBoard,
		    addPiece,
		    deleteFirst,
		    noPieces,
		    positionPiece,
		    lastPiece,
		    firstPiece,
		    pieceAtTile,
		    isSquareFree
) where

import Data.Array
import Sort(quickSort)

type Tile     = (Int,Int)
data ChessSet = Board Int Int Tile Tile (Array Int Int)

instance Eq ChessSet where
    _ == _ = True

instance Ord ChessSet where
    _ <= _ = True			

instance Show ChessSet where
   showsPrec p board@(Board s n l f ts)
      = showString "Move number " . (showsPrec p n).
	showString "\n" . showString (printBoard s (elems ts) 1)

createBoard::Int -> Tile -> ChessSet
createBoard x t = Board x 1 t t onlyFirst
		  where
		     onlyFirst = empty // [(tileIndex x t, 1)]
		     empty     = array (1,x*x) [ (i,0) | i<-[1..x*x]]

sizeBoard::ChessSet -> Int
sizeBoard (Board s _ _ _ _) = s

noPieces::ChessSet -> Int
noPieces (Board _ n _ _ _) = n

addPiece::Tile -> ChessSet -> ChessSet
addPiece t (Board s n l f ts) =Board s (n+1) t f
				    (ts // [(tileIndex s t, n+1)])

deleteFirst::ChessSet -> ChessSet
deleteFirst (Board s n l f ts) = Board s n l l
				       (ts // [(tileIndex s f, 0)])

positionPiece::Int -> ChessSet -> Tile
positionPiece x (Board s _ _ _ ts)
   = findPiece x ts [ i | i<-[1..s*s] ]
     where
        findPiece x ts []     = error "Piece not found"
	findPiece x ts (y:ys) = if ((ts ! y)==x) then (indexTile s y)
			        else
				   findPiece x ts ys

lastPiece::ChessSet -> Tile
lastPiece (Board _ _ l _ _) = l

firstPiece::ChessSet -> Tile
firstPiece (Board _ _ _ f _) = f

pieceAtTile::Tile -> ChessSet -> Int
pieceAtTile x (Board s _ _ _ ts)
   = ts ! (tileIndex s x)

isSquareFree::Tile -> ChessSet -> Bool
isSquareFree x (Board s _ _ _ ts) = (ts ! (tileIndex s x)) == 0



tileIndex:: Int -> Tile -> Int
tileIndex size (x,y) = ((x-1)*size) + y

indexTile::Int -> Int -> Tile
indexTile size x     = ((x `div` size)+1 , x `mod` size)

printBoard s [] i    = []
printBoard s (x:xs) i
   | (i/=s) && (x==0) ="*"     ++(spaces (s*s) 1)++(printBoard s xs (i+1))
   | (i==s) && (x==0) ="*\n"           	         ++(printBoard s xs 1)
   | (i/=s) 	      =(show x)++(spaces (s*s) x)++(printBoard s xs (i+1))
   | (i==s)           =(show x)++ "\n" 	         ++(printBoard s xs 1)

spaces s y = take ((logTen s) - (logTen y) + 1) [' ',' '..]
	     where
	        logTen 1 = 0
		logTen x = 1+ logTen (x `div` 10)

