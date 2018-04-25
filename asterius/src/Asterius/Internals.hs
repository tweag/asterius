{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Asterius.Internals
  ( withSBS
  , withSV
  , encodePrim
  , reinterpretCast
  ) where

import Control.Monad.ST.Strict
import qualified Data.ByteString.Short.Internal as SBS
import Data.Primitive (Prim)
import qualified Data.Primitive as P
import Data.Primitive.ByteArray
import qualified Data.Vector.Storable as SV
import Foreign
import Foreign.C
import GHC.Exts
import GHC.Types

{-# INLINEABLE withSBS #-}
withSBS :: SBS.ShortByteString -> (Ptr CChar -> IO r) -> IO r
withSBS sbs@(SBS.SBS ba) cont =
  if SBS.null sbs
    then cont nullPtr
    else IO
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
withSV v cont =
  if SV.null v
    then cont nullPtr 0
    else SV.unsafeWith v (\p -> cont p (fromIntegral (SV.length v)))

{-# INLINEABLE encodePrim #-}
encodePrim :: Prim a => a -> SBS.ShortByteString
encodePrim a =
  runST
    (do mba <- newByteArray (P.sizeOf a)
        writeByteArray mba 0 a
        ByteArray ba <- unsafeFreezeByteArray mba
        pure (SBS.SBS ba))

{-# INLINEABLE reinterpretCast #-}
reinterpretCast :: (Prim a, Prim b) => a -> b
reinterpretCast a =
  runST
    (do mba <- newByteArray (P.sizeOf a)
        writeByteArray mba 0 a
        readByteArray mba 0)
