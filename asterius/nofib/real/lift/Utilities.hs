
module Utilities(

) where


type Assn key value = [(key, value)]

assLookup :: Eq key => Assn key value -> key -> value
assLookup alist key = head [value | (key',value) <- alist, key == key']

data NameSupply = MkNS Int

initialNameSupply :: NameSupply
initialNameSupply = MkNS 0

newName :: NameSupply -> String -> (NameSupply, String)
newName (MkNS n) prefix = (MkNS (n+1), prefix ++ show n)

data Set a = MkSet [a]

setFromList xs = MkSet (sortNoDups xs)
setEmpty = MkSet []
setSingleton x = MkSet [x]
setToList (MkSet xs) = xs

setUnion (MkSet xs) (MkSet ys) =
  MkSet (merge xs ys) where

setIntersect (MkSet xs) (MkSet ys) =
  MkSet (intersect xs ys) where

setDifference (MkSet xs) (MkSet ys) =
  MkSet (difference xs ys) where

setUnionList ss = foldr setUnion setEmpty ss

data Bag a = MkBag [a]
bagEmpty = MkBag []
bagSingleton x = MkBag [x]
bagFromList xs = MkBag xs
bagToList (MkBag xs) = xs
bagInsert x (MkBag xs) = MkBag (x:xs)
bagUnion (MkBag xs) (MkBag ys) = MkBag (xs ++ ys)

mapAccuml :: (b -> a -> (b, c)) 	-- Function of elt of input list


mapAccuml f b []     = (b, [])
mapAccuml f b (x:xs) = (b'', x':xs') where

sortNoDups :: Ord a => [a] -> [a]
sortNoDups [] = []
sortNoDups (x:xs) = sortNoDups [y | y <- xs, y < x]
