{-# OPTIONS_GHC -Wall -ddump-to-file -ddump-stg -ddump-cmm-raw -ddump-asm #-}

import Data.Foldable
import Data.Traversable
import Foreign.StablePtr

foreign import ccall unsafe "print_i64" print_i64 :: Int -> IO ()

main :: IO ()
main = do
  sps <- for [233 .. 250] newStablePtr
  xs <- for sps deRefStablePtr
  for_ xs print_i64
  for_ sps freeStablePtr
