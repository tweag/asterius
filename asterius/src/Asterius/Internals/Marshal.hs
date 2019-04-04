{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Asterius.Internals.Marshal
  ( marshalSBS
  , marshalV
  ) where

import Control.Monad.Cont
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short.Internal as SBS
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import GHC.Exts
import GHC.ForeignPtr
import GHC.Types

marshalSBS :: ShortByteString -> (forall r. ContT r IO (Ptr CChar))
marshalSBS buf@(SBS.SBS ba)
  | SBS.null buf = pure nullPtr
  | otherwise =
    case SBS.length buf of
      len@(I# l) ->
        ContT $ \c -> do
          fp <- mallocPlainForeignPtrBytes (len + 1)
          withForeignPtr fp $ \ptr@(Ptr p) -> do
            IO $ \s0 -> (# copyByteArrayToAddr# ba 0# p l s0, () #)
            pokeByteOff ptr len (0 :: Word8)
            c ptr

marshalV ::
     forall a. Storable a
  => [a]
  -> (forall r. ContT r IO (Ptr a, Int))
marshalV v
  | null v = pure (nullPtr, 0)
  | otherwise =
    ContT $ \c -> do
      let len = length v
      fp <-
        mallocPlainForeignPtrAlignedBytes
          (sizeOf (undefined :: a) * len)
          (alignment (undefined :: a))
      withForeignPtr fp $ \p -> do
        pokeArray p v
        c (p, len)
