module Main where

import Control.Monad (replicateM_)
import System.Environment (getArgs)
import Prog (prog)

-- #ifdef PAR
-- main input = prog input
-- #else
-- suspect:main ~((Str str):_) = [ReadChan stdin, AppendChan stdout (prog str)]
main = do
	(n:_) <- getArgs
	replicateM_ (read n) $ do
		(s:_) <- getArgs
		length (prog s) `seq` return ()
-- #endif
