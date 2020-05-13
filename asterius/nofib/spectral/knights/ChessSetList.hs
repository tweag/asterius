
module ChessSetList(Tile,
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

import Sort(quickSort)

type Tile     = (Int,Int)
data ChessSet = Board Int Int Tile [Tile]

instance Eq ChessSet where
    _ == _ = True

instance Ord ChessSet where
    _ <= _ = True			

instance Show ChessSet where
   showsPrec p board@(Board sze n f ts)
      = showString (printBoard sze sortedTrail 1)
        where
	   sortedTrail = quickSort
			    (assignMoveNo ts sze n)

createBoard::Int -> Tile -> ChessSet
createBoard x t= Board x 1 t [t]

sizeBoard::ChessSet -> Int
sizeBoard (Board s _ _ _) = s

noPieces::ChessSet -> Int
noPieces (Board _ n _ _) = n

addPiece::Tile -> ChessSet -> ChessSet
addPiece t (Board s n f ts) = Board s (n+1) f (t:ts)

deleteFirst::ChessSet -> ChessSet
deleteFirst (Board s n f ts) = Board s (n-1) (last ts') ts'
			       where
			           ts' = init ts


positionPiece::Int -> ChessSet -> Tile
positionPiece x (Board _ n _ ts) = ts !! (n - x)

lastPiece::ChessSet -> Tile
lastPiece (Board _ _ _ (t:ts)) = t

firstPiece::ChessSet -> Tile
firstPiece (Board _ _ f _) = f

pieceAtTile::Tile -> ChessSet -> Int
pieceAtTile x (Board _ _ _ ts)
   = find x ts
     where
	find x [] = error "Tile not used"
	find x (y:xs)
	   | x == y    = 1 + length xs
	   | otherwise = find x xs

isSquareFree::Tile -> ChessSet -> Bool
isSquareFree x (Board _ _ _ ts) = x `notElem` ts

assignMoveNo [] size x
   = []
assignMoveNo ((x,y):t) size z
   =(((y-1)*size)+x,z):assignMoveNo t size (z-1)

printBoard s [] n
   | (n  > (s*s))   = ""
   | (n `mod` s /=0)= "*"++(spaces (s*s) 1) ++(printBoard s [] (n+1))
   | (n `mod` s ==0)= "*\n"                 ++(printBoard s [] (n+1))
printBoard s trail@((i,j):xs) n
   | (i==n) &&
     (n `mod` s ==0)= (show j)++"\n"++(printBoard s xs (n+1))
   | (i==n) &&
     (n `mod` s /=0)= (show j)++(spaces (s*s) j)++(printBoard s xs    (n+1))
   | (n `mod` s /=0)= "*"     ++(spaces (s*s) 1)++(printBoard s trail (n+1))
   | (n `mod` s ==0)= "*\n"                     ++(printBoard s trail (n+1))

spaces s y = take ((logTen s) - (logTen y) + 1) [' ',' '..]
	     where
	        logTen 0 = 0
		logTen x = 1+ logTen (x `div` 10)

