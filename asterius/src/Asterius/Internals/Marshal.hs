{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.Internals.Marshal
  ( marshalBS,
    marshalV,
  )
where

import Control.Monad.Cont
import qualified Data.ByteString as BS
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import GHC.ForeignPtr

{-# INLINEABLE marshalBS #-}
marshalBS :: BS.ByteString -> (forall r. ContT r IO (Ptr CChar))
marshalBS bs
  | BS.null bs = pure nullPtr
  | otherwise = ContT $ BS.useAsCString bs

{-# INLINEABLE marshalV #-}
marshalV ::
  forall a. Storable a => [a] -> (forall r. ContT r IO (Ptr a, Int))
marshalV v
  | null v = pure (nullPtr, 0)
  | otherwise = ContT $ \c -> do
    let len = length v
    fp <-
      mallocPlainForeignPtrAlignedBytes
        (sizeOf (undefined :: a) * len)
        (alignment (undefined :: a))
    withForeignPtr fp $ \p -> do
      pokeArray p v
      c (p, len)
