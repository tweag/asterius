
module Prime (multiTest) where
import IntLib
import MyRandom

multiTest :: Int -> [Int] -> Integer -> (Bool, [Int])
multiTest k rs n
 = if n <= 1 || even n then (n==2, rs) else mTest k rs
   where mTest 0 rs = (True, rs)
         mTest k rs = if t then mTest (k-1) rs' else (False, rs')
                          where (t, rs') = singleTest n (findKQ n) rs

findKQ :: Integer -> (Integer, Integer)
findKQ n = f (0, (n-1))
           where f (k,q) = if r == 0 then f (k+1, d) else (k, q)
                           where (d, r) = q `divMod` 2

singleTest :: Integer -> (Integer, Integer) -> [Int] -> (Bool, [Int])
singleTest n kq rs
 = (singleTestX n kq (2+x), rs')
   where (x, rs')       = random (n-2) rs

singleTestX n (k, q) x
 = t == 1 || t == n-1 || witness ts
   where (t:ts)         = take (fromInteger k) (iterate square (powerMod x q n))
         witness []     = False
         witness (t:ts) = if t == n-1 then True       else
                          if t == 1   then False      else
                                           witness ts
         square x       = (x*x) `mod` n

random :: Integer -> [Int] -> (Integer, [Int])
random n rs = (makeNumber 65536 (uniform ns rs1), rs2)
              where ns        = chop 65536 n
                    (rs1,rs2) = splitAt (length ns) rs

uniform :: [Integer] -> [Int] -> [Integer]
uniform [n]    [r]    = [toInteger r `mod` n]
uniform (n:ns) (r:rs) = if t == n then t: uniform ns rs
                                  else t: map ((`mod` 65536). toInteger) rs
                        where t  = toInteger r `mod` (n+1)
