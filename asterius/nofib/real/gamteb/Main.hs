--
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
import GamtebMain
import Control.Monad
import System.Environment
import NofibUtils

main = replicateM_ 200 $ do
    (scale:_) <- getArgs
    -- putStr (gamteb (read scale))
    print (hash (gamteb (read scale)))
