module Asterius.Internals.Arena
  ( Arena,
    alloc,
    with,
  )
where

import Asterius.Internals.SafeFromIntegral
import Control.Exception
import Foreign.C
import Foreign.Ptr

newtype Arena = Arena (Ptr Arena)

alloc :: Arena -> Int -> IO (Ptr a)
alloc a len
  | len > 0 = c_arenaAlloc a (safeFromIntegral len)
  | otherwise = fail $ "arenaAlloc: invalid length " <> show len

with :: (Arena -> IO r) -> IO r
with = bracket new free

foreign import ccall unsafe "newArena" new :: IO Arena

foreign import ccall unsafe "arenaAlloc" c_arenaAlloc :: Arena -> CSize -> IO (Ptr a)

foreign import ccall unsafe "arenaFree" free :: Arena -> IO ()
