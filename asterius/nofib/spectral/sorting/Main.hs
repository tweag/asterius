module Main where

import Sort

import Control.Monad (replicateM_)
import Data.List (intersperse)
import System.Environment (getArgs)
import NofibUtils (hash)

main = do
  (n:_) <- getArgs
  replicateM_ (read n) $ do
    (_:s:_) <- getArgs
    f <- readFile s
    print (hash (mangle f))

mangle :: String{-input to sort-} -> String{-output-}
mangle inpt
  = (unlines . sort . lines) inpt
  where
    sort = foldr (.) id (intersperse reverse sorts)
    sorts =
      [ heapSort
      , insertSort
      , mergeSort
      , quickSort
      , quickSort2
      , quickerSort
      , treeSort
      , treeSort2
      ]
