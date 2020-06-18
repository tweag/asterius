{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.Internals.Marshal
  ( marshalBS,
    marshalV,
  )
where

import qualified Asterius.Internals.Arena as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import Foreign
import Foreign.C.Types

marshalBS :: A.Arena -> BS.ByteString -> IO (Ptr CChar)
marshalBS a bs
  | BS.null bs = pure nullPtr
  | otherwise = BS.unsafeUseAsCStringLen bs $ \(p_src, l) -> do
    p_dst <- A.alloc a (l + 1)
    BS.memcpy (castPtr p_dst) (castPtr p_src) l
    pokeByteOff p_dst l (0 :: Word8)
    pure p_dst

marshalV :: forall a. Storable a => A.Arena -> [a] -> IO (Ptr a, Int)
marshalV a v
  | null v = pure (nullPtr, 0)
  | otherwise = do
    let len = length v
    p <- A.alloc a (sizeOf (undefined :: a) * len)
    pokeArray p v
    pure (p, len)
