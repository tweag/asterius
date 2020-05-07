-- Glasgow Haskell 0.403 : FINITE ELEMENT PROGRAM V2
-- **********************************************************************
-- *                                                                    *
-- * FILE NAME : main.hs                DATE : 13-3-1991                *
-- *                                                                    *
-- * CONTENTS : Main program of FEM.                                    *
-- *                                                                    *
-- **********************************************************************

import Database
import Vector
import Displacement
import Elemforce
import PrintSource
import Printuvwforce

import Control.Exception
import Control.Monad
import System.Environment
import NofibUtils (hash, salt)

main = do
  s <- getContents
  (n:_) <- getArgs
  replicateM_ (read n) $
    -- salt s >>= putStr . process
    salt s >>= evaluate . hash . process

process :: String -> String
process s = a
  where
    a  = source_data db ++
         uvwresult db uvwres ++
         forceresult db frc
    db = (idatabase s, rdatabase s)
    uvwres = uvw db
    frc    = forces db uvwres
