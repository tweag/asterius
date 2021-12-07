module Asterius.Internals.Arena
  ( Arena,
    alloc,
    with,
  )
where

import Data.Coerce
import Foreign.Ptr
import Foreign.Marshal.Pool

newtype Arena = Arena Pool

alloc :: Arena -> Int -> IO (Ptr a)
alloc = coerce pooledMallocBytes

with :: (Arena -> IO r) -> IO r
with = withPool . coerce
