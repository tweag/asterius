{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fforce-recomp -Wall -ddump-to-file -ddump-stg -ddump-cmm-raw -ddump-asm #-}

import Control.DeepSeq
import Control.Exception (throwIO)
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import Debug.Trace (trace)
import GHC.Generics
import System.Mem

main :: IO ()
main = do
  let w = (18446744073709551614 :: Word)
  print $ w
  print $ (fromIntegral w :: Integer)
  if show (fromIntegral w) /= show w
    then throwIO . userError $ "cannot create Integer from large Word"
    else pure ()
