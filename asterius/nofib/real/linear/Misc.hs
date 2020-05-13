

module Misc where

takeuntil :: (a ->Bool) -> [a] -> [a]

    {- A variant of takewhile and dropwhile. -}


takeuntil p [] = []
takeuntil p (x:xs)
                  | p x = [x]
                  | otherwise  = x:(takeuntil p xs)


inrange :: Int -> Int -> Int -> Bool
inrange low high x = (low<=x)&&(x<=high)

forceVec :: [Float] -> [Float]
forceVec v =
    case (f v) of
    True -> v
    where
    f [] = True
    f (x:xs) = case (forceFloat x) of
                True -> case (f xs) of
                         True -> True

forceInt :: Int -> Bool
forceInt 0 = True
forceInt x = True

forceFloat :: Float -> Bool
forceFloat 0 = True
forceFloat x = True

forceMat :: [[Float]] -> [[Float]]
forceMat m = map forceVec m



type Runitem = ([Char],[Char])

mkrunitem :: [Char] -> [Char] -> Runitem
mkrunitem output label = (output,label)

run :: Runitem -> [Char]
run (output,label)
   = "START" ++ output ++ "\n" ++ "END"

