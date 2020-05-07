module Main(main) where
import Numbers
import Vectors
import Hide
import MyIO
import EdgePlate	( Input(..) )  -- partain
import Postscript	( Output(..) ) -- partain
import Control.Monad
import System.IO
import System.Environment
import NofibUtils

main = do
  input <- hGetContents stdin
  replicateM_ 20 $ do
    ls <- salt input
    (getFilename $
      process (\viewdir -> hiddenline viewdir. map read. lines)) (lines ls)
