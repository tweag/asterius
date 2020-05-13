
module Input
(gmat,rhside,soln_vect,wells,split,blm',mkbigvec,
              mksparse,a_easy,a_hard,x1) where


import Data.List (transpose)
import Matrix
import AbsDensematrix
import Utils



mkbigvec :: [[Float]] -> Vector
mkbigvec v = mkvector (map mkvec v)

mksparse :: [[(Int,Int,[[Float]])]] -> Matrix
mksparse m
   = mkmatrix (map (map f) m)
     where
	f (r,c,b) = (r-1,c-1,mkblock b)

directory :: String
directory = "./Data/"

file1 :: Int -> String
file1  n = directory ++ "gcomp.mat" ++ (show n) ++ ".one"

file2a :: Int -> String
file2a n = directory ++ "gcomp.mat" ++ (show n) ++ ".twoa"

file2b :: Int -> String
file2b n = directory ++ "gcomp.mat" ++ (show n) ++ ".twob"

file2c :: Int -> String
file2c n = directory ++ "gcomp.mat" ++ (show n) ++ ".twoc"

file3 :: Int -> String
file3  n = directory ++ "gcomp.mat" ++ (show n) ++ ".three"

file4 :: Int -> String
file4  n = directory ++ "gcomp.mat" ++ (show n) ++ ".four"

file5 :: Int -> String
file5  n = directory ++ "gcomp.mat" ++ (show 2) ++ ".soln"

file6 :: Int -> String
file6  n = directory ++ "gcomp.mat" ++ (show n) ++ ".well"

file :: String
file   = "gconem.bmtx3"  -- original file

resn = 11
resm = 21
resnm = resn * resm

readmtx rtype x
      = map (map numval) (map f y)
        where
               y = if (rtype == 1) then drop 1 (lines ("abc"))
                   else
                        if (rtype == 2) then init (lines ("abc"))
                        else  lines ("abc")

               f y   = [take 15 (drop 1  y ) ,
                        take 15 (drop 16 y ) ,
                        take 15 (drop 31 y ) ,
                        take 15 (drop 46 y ) ]
readsoln x
      = map (map numval) (map f y)
        where  y = lines ("abc")
               f y   = [take 15 (drop 8  y ) ,
                        take 15 (drop 24 y ) ,
                        take 15 (drop 40 y ) ,
                        take 15 (drop 56 y ) ]


myreadmtx rtype x
      = map (map numval) (map f y)
        where  y = if (rtype == 1) then drop 1 (lines (myread x))
                   else
                        if (rtype == 2) then init (lines (myread x))
                        else lines (myread x)
               f y   = [take 15 (drop 1  y ) ,
                        take 15 (drop 16 y ) ,
                        take 15 (drop 31 y ) ,
                        take 15 (drop 46 y ) ]

myread fname
   = "abc"



readsolnvect x
    = collect_by4 (concat z)
      where z = map (map numval) (map f y)
            y = lines ("abc")
            f y   = [take 21 (drop 11 y )]

collect_by4 [] = []
collect_by4 (a:b:c:d:xs)
    = [a,b,c,d]: (collect_by4 xs)

split m []        = []
split m xs        = (take m xs):(split m (drop m xs))
splitmtx n m xs = split n (split m xs)

input1'  n = readmtx 1 (file1 n)
input1   n = split 4 (input1' n)

input2a' n = readmtx 0 (file2a n)
input2a  n = split 4 (input2a' n)

input2b' n = readmtx 0 (file2b n)
input2b  n = split 4 (input2b' n)

input2c' n = readmtx 0 (file2c n)
input2c  n = split 4 (input2c' n)

input3'  n = readmtx 0 (file3  n)
input3   n = split 4 (input3'  n)

input4'  n = readmtx 2 (file4  n)

input5'  n = readsolnvect (file5 n)

lowerband' n =  take (resnm-resn) (drop resn (input1 n))
lowerband n = map transpose (lowerband' n)
block_lowerband n = convert_lowerband (lowerband' n)

ldiagband' n  = drop 1 (input2a n)
ldiagband n = map transpose (ldiagband n)
block_ldiagband n = convert_ldiagband (ldiagband' n)

mdiagband'  = input2b
mdiagband n = map transpose (mdiagband' n)
block_mdiagband n = convert_mdiagband (mdiagband' n)

udiagband' n = init(input2c n)
udiagband n = map transpose (udiagband' n)
block_udiagband n = convert_udiagband (udiagband' n)

upperband' n =  take (resnm-resn) (input3 n)
upperband n = map transpose (upperband' n)
block_upperband n = convert_upperband (upperband' n)

rhside n = mkbigvec (input4' n)

soln_vect n = mkbigvec (input5' n)

convert_lowerband :: [[[Float]]] -> Block_list
convert_lowerband b
        = [(i+resn,i,head (drop(i-1) b) ) | i <-[1..length b]]


convert_ldiagband :: [[[Float]]] -> Block_list
convert_ldiagband b
        = [(i+1,i,head (drop(i-1)b)) | i <-[1..length b]]




convert_mdiagband :: [[[Float]]] -> Block_list
convert_mdiagband b
        = [(i,i,head (drop(i-1)b)) | i <-[1..length b]]




convert_udiagband :: [[[Float]]] -> Block_list
convert_udiagband b
        = [(i,i+1,head (drop(i-1)b))| i <-[1..length b]]


convert_upperband :: [[[Float]]] -> Block_list
convert_upperband b
        = [(i,i+resn, head (drop(i-1) b)) | i <-[1..length b]]

wells :: Int -> Int
wells 6 = 1
wells 7 = 3
wells 8 = 7
wells n = 0

gvect :: Vector
gvect = mkbigvec (rep resnm [1,1,1,1])

gmat n  = make_lmat (block_lowerband n)
                    (block_ldiagband n) (block_mdiagband n) (block_udiagband n)
                    (block_upperband n)

gvect0 :: Vector
gvect0 = mkbigvec (rep resnm [0,0,0,0])


make_lmat :: Block_list -> Block_list -> Block_list ->
             Block_list -> Block_list -> Matrix
make_lmat lower ldiag mdiag udiag  upper
   = mksparse [row i | i<- [1..resnm]]
   where
   row i = combine i lower ldiag mdiag udiag  upper
   combine i low ld md ud up =
                  if (i==1) then (take 1 md) ++ (take 1 ud) ++ (take 1 up)
                  else if (i <=resn) then  diag3 ++ upper
                        else if (i >(resnm - resn)) then lower ++ diag3
                              else if (i == resnm) then
                                   [last low] ++ [last ld] ++ [last md]
                                    else lower ++ diag3 ++ upper
     where
         diag3 = (take 1 (drop (i-2) ld ))  ++
                  (take 1 (drop (i-1) md ))  ++
                  (take 1 (drop (i-1) ud ))
         upper = take 1(drop(i-1) up)
         lower = take 1(drop (i-resn-1) low)

x0 size = mkbigvec [ [0,0,0,0] | i<-[1..size*size]]
x1 size = mkbigvec [ [1,1,1,1] | i<-[1..size*size]]

a_easy size
   = if (size > 1) then blm size
     else  mksparse [[(1,1,[[11,-1, 0, 0],
                      [-1,11,-1, 0],
                      [ 0,-1,11,-1],
                      [ 0, 0,-1,11]])]]

a_hard size
    = if size > 1 then blm_hard size
      else  error "ummm. system size to small for this model."



off_block  =         [[-1,0,0,0],
                     [0,-1,0,0],
                     [0,0,-1,0],
                     [0,0,0,-1]]

d_block    =         [[11,-1, 0, 0],
                     [-1,11,-1, 0],
                     [ 0,-1,11,-1],
                     [ 0, 0,-1,11]]

hard_off_block  = (map (map (/90))
                     [[-1,-2,-3,-4],
                      [-4,-1,-2,-3],
                      [-3,-4,-1,-2],
                      [-2,-3,-4,-1]])

hard_d_block    = (map (map (/90))
                     [[90,-2,-3,-4],
                      [-4,90,-2,-3],
                      [-3,-4,90,-2],
                      [-2,-3,-4,40]])

blm  :: Int -> Matrix
blm n = mksparse (blm' n)

blm' :: Int -> [[(Int,Int,[[Float]])]]
blm' n = [row i | i<- [1..(n*n)]]
         where row i = br i n


br :: Int -> Int -> [(Int,Int,[[Float]])]
br rw n =
           if ((rw == 1) ||  (rw == (n*n-n+1))) then
            [block i| i<- [1,2,n+1,n+2]]

           else if ((rw == n) || (rw == n*n)) then [block i | i<-[n-1,n,2*n-1,2*n]]

           else if ((rw `mod` n) == 1) then [block i | i<-[1,2,n+1,n+2,2*n+1,2*n+2]]

           else if ((rw `mod` n) == 0) then [block i | i<-[n-1,n,2*n-1,2*n,3*n-1,3*n]]

           else if ((rw >1) && (rw <n)) || ((rw >(n*n-n+1)) && (rw < n*n)) then [block i | i<-[m,1+m,2+m,n+m,n+1+m,n+2+m]]

            else [block i | i<-[m,1+m,2+m,n+m,n+1+m,n+2+m,
                 2*n+m,2*n+1+m,2*n+2+m]]

           where
             block i = if (rw == (i+offset)) then (rw,i+offset,d_block)
                       else (rw,i+offset,off_block)
             m = (rw `mod` n) -1
             offset = if (rw <= n) then 0
                      else (((rw-1) `div` n) -1) * n


blm_hard  :: Int -> Matrix
blm_hard n
   = mksparse hard
     where
        hard = map (map f) (blm' n)
        f (r,c,b) = if (r/=c) then (r,c,hard_off_block)
                    else  (r,c,hard_d_block)

include_wells m (a:as) (b:bs) (c:cs) (d:ds)

   = if (as == []) then include_well m a b c d
     else  include_wells m' as bs cs ds
     where m' = include_well m a b c d

include_well m well_pos wellg wellh welldw
         = include_dw m'' welldw
           where m'' = include_col_well m' well_pos wellg
                 m'  = include_row_well m  well_pos wellh

include_row_well m well_pos wellh
    = m'
      where m' = m ++ [new_row]
            new_row = map2 f well_pos (split 4 wellh)
            f x y = (row_num, x,[y])
            row_num = length m +1


include_col_well m well_pos wellg
    = foldl append_row m x
      where x = zip2 well_pos (split 4 wellg)


include_dw m welldw = append_row m (length m,welldw)


append_row m (rownum, dat)
    = m'
      where m' = (take (rownum-1) m) ++
                    [appendrow]            ++
                    drop rownum m
            appendrow = row ++ [(rownum,colnum,data')]
            data' = split 1 dat
            row = m !! (rownum-1)
            colnum = length m


numval :: String -> Float
numval x = (read x)::Float
