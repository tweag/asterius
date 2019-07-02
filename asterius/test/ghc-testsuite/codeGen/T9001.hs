{-# LANGUAGE RankNTypes #-}
import System.Mem


newtype FMList = FM {unFM :: forall m. m -> m }

-- On protecting the first megablock: 
-- (if (!bds.has(this.all_bds[i]) && this.all_bds[i] != 9007160603181312n))
-- 500 +
-- 503 +
-- 504 +
-- 505 +
-- what happens between 505 and 506??
-- 506 -
-- 507 -
-- 515 -
-- 525 -
-- 550 -
-- 600 - 

-- Without protecting the first megablock
-- 300 +
-- 350 +
-- 355 +
-- 356 +
-- FAILURE HERE
-- 357 -
-- 360 -
-- 375 -
-- 400 -
-- 500 -

main = print (delete 357 (FM id) :: Int) >> performGC

delete :: Int -> FMList -> Int
delete 0 _ = 0
delete n (FM a) = a $ delete (n-1) $ FM $ \g -> a (const g) undefined

-- g :: forall m. m -> m
-- a :: forall n. n -> n
-- const :: a -> b -> a
-- const g :: b -> (forall m. m -> m)
-- a (const g) :: b -> (forall m. m -> m)
-- a (const g) undefined :: forall m. m -> m -- | Holds the g forever? is that all we want? a cycle?
-- I don't understand what this triggers in the GC...
