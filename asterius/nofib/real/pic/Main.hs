--
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
import	Pic
import  PicType	-- added by partain
import Control.Exception(evaluate)
import Control.Monad(replicateM_)
import System.IO(hPutStr,stderr)
import System.Environment(getArgs)
import NofibUtils (hash,salt)

main = do
    (n:_) <- getArgs
    replicateM_ (read n) $ do
		(_:s:_) <- getArgs
		evaluate (hash (takeWhile ((/=) '\n') s ++ pic (read s)))
