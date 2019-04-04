{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Asterius.Internals.Marshal
  ( Pool
  , withPool
  , marshalSBS
  , marshalV
  ) where

import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short.Internal as SBS
import Data.IORef
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import GHC.Exts
import GHC.ForeignPtr
import GHC.Types

newtype Pool =
  Pool (IORef [ForeignPtr ()])

withPool :: (Pool -> IO r) -> IO r
withPool c = do
  r <- newIORef []
  c $ Pool r

marshalSBS :: Pool -> ShortByteString -> IO (Ptr CChar)
marshalSBS (Pool r) buf@(SBS.SBS ba)
  | SBS.null buf = pure nullPtr
  | otherwise =
    case SBS.length buf of
      len@(I# l) -> do
        fp <- mallocPlainForeignPtrBytes (len + 1)
        modifyIORef' r (castForeignPtr fp :)
        case unsafeForeignPtrToPtr fp of
          ptr@(Ptr p) -> do
            IO $ \s0 -> (# copyByteArrayToAddr# ba 0# p l s0, () #)
            pokeByteOff ptr len (0 :: Word8)
            pure ptr

marshalV ::
     forall a. Storable a
  => Pool
  -> [a]
  -> IO (Ptr a, Int)
marshalV (Pool r) v
  | null v = pure (nullPtr, 0)
  | otherwise = do
    let len = length v
    fp <-
      mallocPlainForeignPtrAlignedBytes
        (sizeOf (undefined :: a) * len)
        (alignment (undefined :: a))
    modifyIORef' r (castForeignPtr fp :)
    let p = unsafeForeignPtrToPtr fp
    pokeArray p v
    pure (p, len)
