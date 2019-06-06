{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fforce-recomp -Wall -ddump-to-file -ddump-stg -ddump-cmm-raw -ddump-asm #-}

import Control.DeepSeq
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import GHC.Generics
import System.Mem
import Debug.Trace (trace)

main :: IO ()
main = do
    let w = (18446744073709551614 :: Word)
    print $ w
    print $ (fromIntegral w :: Integer)
