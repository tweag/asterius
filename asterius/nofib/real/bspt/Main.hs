module Main where
import Prog (prog)
import Control.Monad (replicateM_)
import System.Environment (getArgs)
import NofibUtils (hash, salt)

main = do
    str <- getContents
    [n] <- getArgs
    replicateM_ (read n) $ do
        str' <- salt str
        print (hash (prog str'))
