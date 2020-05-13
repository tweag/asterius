
module Stdlib

		middle,mkset,between,pair,--UNUSED: pairUp,curry,
		{-const,-}const3,splitAt_YORK,numval,toNum,all_YORK)

where

mapcat :: (a->[b]) -> [a] -> [b]
mapcat f x = foldr (++) [] (map f x)

map2 :: (a->b->c) -> [a] -> [b] -> [c]
map2 _ [] l = []
map2 _ l [] = []
map2 f (x:xs) (y:ys) = (f x y):(map2 f xs ys)

mkset [] = []
mkset (a:l) = a:mkset ((filter ((/=) a)) l) -- "remove" no longer in prelude

mappair :: (a->b) -> (a,a) -> (b,b)
mappair f (a,b) = (f a,f b)

middle :: Int-> Int-> Int
middle x y = div (x+y) 2

between :: (Ord a,Num a) => a -> a -> a -> Bool
between base offset value = (base <= value) && (value <= (base+offset))

pair :: a -> b -> (a,b)
pair x y = (x,y)

{-
const :: a -> b -> a
const x y = x
-}

const3 :: a -> b -> c -> d -> a
const3 w x y z = w

splitAt_YORK ::(Eq a) => a -> [a] -> ([a],[a])
splitAt_YORK ch [] = ([],[])
splitAt_YORK ch (a:l) | a==ch = ([], l)
splitAt_YORK ch (a:l) | otherwise = (a:x,y)
                        where (x,y) = splitAt_YORK ch l

numval :: String -> Int
numval x = numval' (length x-1) x
                where numval' _ [] = 0
                      numval' x (a:l) = (toNum a)*(10^x) + (numval' (x-1) l)

toNum :: Char -> Int
toNum x = (fromEnum x - fromEnum '0')

seQuence :: [String] -> String
seQuence = concat

all_YORK :: [Bool] -> Bool
all_YORK = and
