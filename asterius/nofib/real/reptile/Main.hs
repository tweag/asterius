-- LML original: Sandra Foubister, 1990
-- Haskell translation: Colin Runciman, May 1991

module Main(main) where

import Mgrfuns
import Progfuns
import Auxprogfuns
import Layout
import Tilefuns

import Control.Monad
import System.Environment
import NofibUtils

main = do
    input <- getContents
    replicateM_ 500 $ do
	fromMgr <- salt input
	let
	      toMgr = setmode 7 ++
		      shapewindow [0,0,1150,900] ++
		      setup ++
		      potatotile ([],1,initalist) (lines fromMgr) ++
		      shapewindow [0,0,500,500] ++
		      font 8 ++
		      textreset ++
		      clear ++
		      func 15
	print (hash toMgr)



