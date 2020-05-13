
module KnightHeuristic(
	ChessSet,
	startTour,
	descendents,
	tourFinished
) where

import Sort(quickSort)
import ChessSetList

data Direction = UL | UR | DL |DR | LU | LD | RU | RD

move::Direction -> Tile -> Tile
move UL (x,y) = (x-1,y-2)
move UR (x,y) = (x+1,y-2)
move DL (x,y) = (x-1,y+2)
move DR (x,y) = (x+1,y+2)
move LU (x,y) = (x-2,y-1)
move LD (x,y) = (x-2,y+1)
move RU (x,y) = (x+2,y-1)
move RD (x,y) = (x+2,y+1)

startTour::Tile -> Int -> ChessSet
startTour st size
   | (size `mod` 2) == 0 = createBoard size st
   | otherwise           = error "Tour doesnt exist for odd size board"

moveKnight::ChessSet -> Direction -> ChessSet
moveKnight board dir
   = addPiece (move dir (lastPiece board)) board

canMove::ChessSet -> Direction -> Bool
canMove board dir
   = canMoveTo (move dir (lastPiece board)) board

canMoveTo::Tile -> ChessSet -> Bool
canMoveTo t@(x,y) board
   = (x >= 1) && (x <=sze) &&
     (y >= 1) && (y <=sze) &&
     isSquareFree t board
     where
        sze = sizeBoard board

descendents::ChessSet -> [ChessSet]
descendents board
   | (canJumpFirst board) &&
     (deadEnd (addPiece (firstPiece board) board)) = []
   | otherwise          = case (length singles) of
			     0  -> map snd (quickSort (descAndNo board))
			     1  -> singles
			     _  -> []		-- Going to be dead end
		          where
		             singles = singleDescend board

singleDescend::ChessSet -> [ChessSet]		
singleDescend board =[x | (y,x) <- descAndNo board, y==1]

descAndNo::ChessSet -> [(Int,ChessSet)]
descAndNo board
   = [(length (possibleMoves (deleteFirst x)),x) | x<- allDescend board]

allDescend::ChessSet -> [ChessSet]
allDescend board
   =  map (moveKnight board) (possibleMoves board)

possibleMoves::ChessSet -> [Direction]
possibleMoves board
   =[x | x <- [UL,UR,DL,DR,LU,LD,RU,RD], (canMove board x)]

deadEnd::ChessSet -> Bool
deadEnd board = (length (possibleMoves board)) == 0

canJumpFirst::ChessSet -> Bool
canJumpFirst board
  = canMoveTo (firstPiece board) (deleteFirst board)

tourFinished::ChessSet -> Bool
tourFinished board
   = (noPieces board == sze*sze) && (canJumpFirst board)
     where
        sze = sizeBoard board
