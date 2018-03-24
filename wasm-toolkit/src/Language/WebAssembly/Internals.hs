{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Language.WebAssembly.Internals
  ( withSBS
  , withSV
  ) where

import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBS
import qualified Data.Vector.Storable as SV
import Foreign
import Foreign.C
import GHC.Exts
import GHC.Types

{-# INLINEABLE withSBS #-}
withSBS :: SBS.ShortByteString -> (Ptr CChar -> IO r) -> IO r
withSBS (SBS.SBS ba) cont =
  IO
    (\s0 ->
       case newPinnedByteArray# (l0 +# 1#) s0 of
         (# s1, mba #) ->
           case copyByteArray# ba 0# mba 0# l0 s1 of
             s2 ->
               case writeWord8Array# mba l0 0## s2 of
                 s3 ->
                   case unsafeFreezeByteArray# mba s3 of
                     (# s4, ba' #) ->
                       case cont (Ptr (byteArrayContents# ba')) of
                         IO cf -> cf s4)
  where
    l0 = sizeofByteArray# ba

{-# INLINEABLE withSV #-}
withSV :: (Storable a, Num n) => SV.Vector a -> (Ptr a -> n -> IO r) -> IO r
withSV v cont = SV.unsafeWith v (\p -> cont p (fromIntegral (SV.length v)))
