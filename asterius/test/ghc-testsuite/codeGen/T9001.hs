{-# LANGUAGE RankNTypes #-}

newtype FMList = FM {unFM :: forall m. m -> m }

main = print (delete 1000 (FM id) :: Int)

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
