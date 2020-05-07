module Main where

import Prog (prog)
import System.Environment

main = do
    (n:_) <- getArgs
    mapM_ (putStr . prog) [1..read n]
