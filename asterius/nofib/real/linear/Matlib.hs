
module Matlib (scale, precond, uncondition) where

import Data.List (genericLength)
import Matrix
import AbsDensematrix


type Scale_function   = Matrix -> Vector -> (Matrix,Vector)
type Precond_function = Matrix -> Vector -> Vector

type Block_tuple = (Int,Int,Block)


scale       :: Scale_function
precond     :: Int -> Precond_function
uncondition :: Precond_function

scale = bdscl


uncondition a v = mvmult (diag_matrix a) v


bdscl :: Matrix -> Vector -> (Matrix,Vector)
bdscl m vs
   = (m', vs')
     where
        vs' = mkvector [ bvecmult (row_factor r) (vsubscript r vs)
                          | r <- [0..(vsize vs)-1] ]
        m' = mkmatrix [map scale1 (getrow i m) | i <- [0..(numrows m)-1]]

        scale1 (r,c,b) = if r==c then (r,c, bident (fst (bsize b)))
                         else (r,c, bmult (row_factor r) b)
        row_factor n = if (okindex n diag_inverses) then  diag_inverses !! n
                       else error "bdscl:dinverse in matrix"

	diag_inverses = map inverse (diag_blocks m)
        inverse (r,c,b) = binverse b

precond nwells a v
    = backsubs (pseudofactor firstwell a) v
    where
        firstwell = (numrows a) - nwells


pseudofactor :: Int -> Matrix -> Matrix
pseudofactor n mat
   = mat'
     where
        mat' = mkmatrix [ newrow k | k <- blocknums mat ]
        newrow k = (bilu k) ++ (bclu k)
        maxblock = last (blocknums mat)
        bilu k = if (k>=n) then []
                 else  map bilu' (colsbefore n (getrow k mat))
        bilu' (r,c,oldb)
           = if r/=c then (r,c,oldb)
             else  (r,c,binverse newb)
              where
                 newb = if sumpart == [] then oldb
                       else  oldb `bsub` (bsum sumpart)
                 k=r
                 sumpart = [ b'uk `bmult` (b' k k) `bmult` (b' k u)
                           | (u,k,b'uk) <- colsbefore k (getrow k mat') ]
        b  i j = msubscript i j mat
        b' i j = msubscript i j mat'

        first_col_in_row r
           = if getrow r mat == [] then  maxblock
             else colno (head (getrow r mat))
        colno (r,c,b) = c
        firstcols = [first_col_in_row r | r<-[n..maxblock]]
        firstcol r = firstcols !! (r-n)
        exist r c
           = if c>r then exist c r
             else if c>=(firstcol r) then True
                   else False

        bclu r
           = if (r<n) then [ bclu' r c  | c <- [n..maxblock], exist r c ]
             else [ bclu' r c  | c <- blocknums mat, exist r c ]
        bclu' i j
           = if result == [] then error "pseudofactor"
             else if i==j then (i,j,binverse (head result))
             else (i,j,head result)
             where
             result =  if i<j then [b' i i] `mult` ([b i j] `sub` sumpartU)
                      else [b i j] `sub` sumpartL
             sumpartU = sumblocks [ [bik] `mult` [b k j]
                                      | (x,k,bik) <- (getrow i mat), k<i ]
             sumpartL = sumblocks [ [bik] `mult` [b' k k] `mult` [b' k j]
                                      | (x,k,bik) <- (getrow i mat), k<j ]



colsbefore n row = [(r,c,b) | (r,c,b) <- row, c < n ]



backsubs m v
   = v'
      where
         v' = ((backward m) . (forward m))  v



forward m rs
   = mkvector [(y k) | k <- (blocknums m) ]
     where
        y k = bvecmult (dinv k) (terms k)
        dinv k = b k k
        r k = vsubscript k rs
        terms k = if (sumpart k)==[] then (r k)
                  else  (r k) `vecsub` (vecsum (sumpart k))
        sumpart k = sumparts!!k
        sumparts = [ [ b_ij `bvecmult` (y j) | (i,j, b_ij) <- (getrow k m) , j<i ]
                      | k <- (blocknums m) ]
        b i j = msubscript i j m




backward :: Matrix -> Vector -> [Vec]
backward m ys
   = mkvector ps
     where
       ps = [ (p' k) | k <- (blocknums m) ]
       p' k =  if (terms k) == [] then (y k)
              else (y k) `vecsub` ((b k k) `bvecmult` (vecsum (terms k)))
       p k = ps !! k
       b i j = msubscript i j m
       y k = vsubscript k ys
       termss  = [ [b_i_j `bvecmult` (p j) | (i,j, b_i_j) <- (getrow k m), j>i ]
                   | k <- (blocknums m) ]
       terms k = termss!!k

mult [ ] [ ] = []
mult [x] [ ] = []
mult [ ] [y] = []
mult [x] [y] = [bmult x y]

sub  [ ] [ ] = []
sub  [x] [ ] = [x]
sub  [ ] [y] = [bneg y]
sub  [x] [y] = [bsub x y]

sumblocks xs = if blocks /= [] then [bsum blocks]
               else []
               where blocks = concat xs

bsum :: [Block] -> Block
bsum [] = error "bsum: no blocks to sum"
bsum ms = foldl1 badd ms

diag_matrix :: Matrix -> Matrix
diag_matrix m = mkmatrix [filter is_diag (getrow i m) | i <- blocknums m]

diag_blocks :: Matrix -> [Block_tuple]
diag_blocks m
   = if (length(diags) == numrows m) then concat diags
     else  error "matlib:diag_blocks:  given matrix with missing diagonal block"
     where
	diags = [filter is_diag (getrow i m) | i <- blocknums m]

is_diag :: Block_tuple -> Bool
is_diag (r,c,b) = r==c


blocknums :: Matrix -> [Int]
blocknums m = [0..(numrows m)-1]


vecsum :: [Vec] -> Vec
vecsum [] = error "vecsum: no vecs to sum"
vecsum ms = foldl1 vecadd ms

okindex :: Int -> [a] -> Bool
okindex n m = (0<=n)&&(n<=((length m)-1))

testmat :: Matrix -> [Char]
testmat m
   = if result == [] then  "Matrix is probably ok"
     else result
     where
	result = (rowsnumbered m) ++ (symmetric m) ++ (sorted m)

rowsnumbered :: Matrix -> [Char]
rowsnumbered m
   = concat [ goodrow i (getrow i m) | i <- blocknums m ]
     where
	goodrow i row = concat  (map (isrow i) row)
	isrow i (r,c,b)
	   =if i==r then  []
	    else  ("Row " ++ (show i) ++ " is misnumbered\n")

symmetric :: Matrix -> [Char]
symmetric m
   = concat [ symmetric_row (getrow i m) | i <- blocknums m ]
     where
	symmetric_row row = concat [ has_corresponding elem | elem <- row ]
	has_corresponding (r,c,b)
	   = if exists c r then []
	     else "Cannot find corresponding block for " ++ (show (r,c))++"\n"
	exists r c = (filter (iscol c) (getrow r m)) /= []
	iscol c (i,j,b) = c==j

sorted :: Matrix -> [Char]
sorted m
   = concat [ sortedrow i (getrow i m) | i <- blocknums m ]
     where
	sortedrow i row
	   = if row == (sort row) then []
	     else "Row number " ++ (show i) ++ " is not properly sorted.\n"

sort :: (Ord a) => [a] -> [a]
sort xs = if (n == 1) then xs
          else merge (sort us) (sort vs)
          where
           n = genericLength xs
           us = take (n `div` 2) xs
           vs = drop (n `div` 2) xs


merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge (x:xs) [] = (x:xs)
merge (x:xs) (y:ys) = if (x <= y) then (x:merge xs (y:ys))
                    else (y:(merge (x:xs) ys))
