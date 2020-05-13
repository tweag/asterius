
module IntLib (readInteger, showInteger, makeNumber, chop,
               powerMod, cubeRoot, log2) where
import Data.List--1.3
rcsid = "$Header: /srv/cvs/cvs.haskell.org/fptools/nofib/spectral/primetest/IntLib.lhs,v 1.2 1996/07/25 21:32:53 partain Exp $"

readInteger :: String -> Integer
readInteger s = read s

showInteger :: Integer -> String
showInteger i = show i

makeNumber :: Integer -> [Integer] -> Integer
makeNumber b = foldl f 0 where f a x = a * b + x

chop :: Integer -> Integer -> [Integer]
chop b = chop' [] where chop' a n = if n == 0 then a else chop' (r:a) q
                                    where (q,r) = n `divMod` b

powerMod :: Integer -> Integer -> Integer -> Integer
powerMod a 0 m = 1
powerMod a b m
 = f a' (b-1) a'
   where a' = a `mod` m
         f a 0 c = c
         f a b c = g a b where
                   g a b | even b    = g ((a*a) `mod` m) (b `div` 2)
                         | otherwise = f a (b-1) ((a*c) `mod` m)

cubeRoot :: Integer -> Integer
cubeRoot x = until satisfy improve x
             where satisfy y = y*y*y >= x && y'*y'*y' < x where y' = y-1
                   improve y = (2*y*y*y+x) `ddiv` (3*y*y)
                   ddiv a b  = if (r < b `div` 2) then q else q+1
                               where (q,r) = divMod a b

log2 :: Integer -> Integer
log2 = genericLength . chop 2
