

module Matrix
         (Matrix, Vector, Block , Vec ,
          Block_list , Row_pos, Col_pos,
          mmult, mvmult, svmult,
          madd, msub,
          vadd, vsub,
          vdot, vouter,
          mneg, vneg,
          norm,
          mkmatrix,
          mkvector,
          mergevectors,
          mupdate, vupdate,
          msubscript, vsubscript,
          getrow,
          getcol,
          numrows,
          msize, vsize,
          showmatrix, showvector) where


import AbsDensematrix
import Utils



mmult   :: Matrix -> Matrix -> Matrix
madd    :: Matrix -> Matrix -> Matrix
msub    :: Matrix -> Matrix -> Matrix
mvmult  :: Matrix -> Vector -> Vector
vadd    :: Vector -> Vector -> Vector
vsub    :: Vector -> Vector -> Vector
vdot    :: Vector -> Vector -> Scalar
vouter  :: Vector -> Vector -> Matrix
norm    :: Vector -> Scalar
mneg    :: Matrix -> Matrix
vneg    :: Vector -> Vector
svmult  :: Scalar -> Vector -> Vector

mupdate :: Matrix -> (Int,Int) -> Block -> Matrix
vupdate :: Vector -> Int -> Vec -> Vector

msubscript  :: Int -> Int -> Matrix -> Block
msubscript' :: Int -> Int -> Matrix -> [Block]
vsubscript  :: Int -> Vector -> Vec
getrow      :: Int -> Matrix -> [Block_tuple]
getcol      :: Int -> Matrix -> [Block_tuple]
numrows     :: Matrix -> Int
msize       :: Matrix -> (Int,Int)
vsize       :: Vector -> Int

mkmatrix   :: [[(Int,Int,Block)]] -> Matrix
mkvector   :: [Vec] -> Vector
mergevectors :: Vector -> Vector -> Vector

showmatrix :: Matrix -> [Char]
showvector :: Vector -> [Char]

type Row_pos        = Int
type Col_pos        = Int
type Block_tuple    = (Row_pos, Col_pos, Block)
type Block_list     = [Block_tuple]

type Matrix = Matrix_type
type Vector = Vector_type

type Matrix_type = [Block_list]
type Vector_type = [Vec]

type Scalar = Float
sadd       = (+)

mmult m1 m2 = error "unsupported matrix operation"

madd m1 m2 = error "unsupported matrix operation"

msub m1 m2 = error "unsupported matrix operation"


mneg m = map (map negtuple) m
                    where
                       negtuple (r, c, b) = (r, c, bneg b)


vadd v1 v2   = map2 vecadd v1 v2

vsub v1 v2   = map2 vecsub v1 v2

vdot v1 v2   = foldl1 sadd (map2 vecdot v1 v2)

vouter v1 v2   = error "unsupported vector operation"

norm v   = foldl1 sadd (map vecnorm v)

vneg v   = map vecneg v

svmult s v = map (svecmult s) v

mupdate m (i,j) val
   = [getrow k m |k <- [0..i-1]] ++ [(f(m!!i))]
      ++ [getrow l m | l <- [(i+1) .. (numrows m)-1]]
     where
        f xs = (take j xs) ++ [(i, (j+1), val)] ++ (drop (j+1) xs)

	
vupdate v i vc = (take i v) ++ [vc] ++ (drop (i+1) v)


showmatrix m
   = concat [ (showrow i)++"\n" | i<-[0..length(m)-1] ]
     where
	showrow i = concat [status i j | j<-[0..length(m)-1]]
	status i j
	   = if exist i j then "#"
	     else "."
	exist i j = j `elem` (row i)
	row i = [ col | (r,col,b) <- (m!!i) ]

showvector vs =  concat (map showvec vs)



mkmatrix = id


mkvector = id

mergevectors = (++)



mvmult rows v
   =  if ok then [ rvdot row v | row <- rows ]
      else error "Incompatible operands to large mvmult"
     where
        ok = (length rows) == (length v)

rvdot row v
     = foldl1 vecadd [bvecmult b (vsubscript c v) | (r,c,b) <- row]



okindex :: Int -> [a] -> Bool
okindex n m = (0<=n) && (n<=((length m) - 1)) -- testing (irm)


iscol :: Int -> Block_tuple -> Bool
iscol k (r,c,b) = (k==c)


msubscript' r c m
   = map strip_block (filter (iscol c) (getrow r m))

msubscript r c m
   = if thingee /= [] then (head thingee)
    else zero_block r c m
     where
        thingee = msubscript' r c m


getrow n m
   = if okindex n m then m !! n
     else error "getrow: index out of bounds"



getcol n m
   = concat [ filter (iscol n) row | row <- m ]


numrows m = length m

msize m = (length m,length (head m))

vsize v = length v



strip_block :: Block_tuple -> Block
strip_block (r,c,b) = b


vsubscript n v
   = if okindex n v then v!!n
     else error "vsubscript in matrix"



zero_block :: Int -> Int -> Matrix -> Block
zero_block i j m
   = mkblock (rep nrows (rep ncols 0))
     where
       (nrows,junk1) = block_size (head (getrow i m))
       (junk2,ncols) = block_size (head (getcol j m))
       block_size (r,c,b) = bsize b

