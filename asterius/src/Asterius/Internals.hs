{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}

module Asterius.Internals
  ( IO
  , marshalSBS
  , marshalV
  , encodeStorable
  , reinterpretCast
  , encodeFile
  , decodeFile
  , tryDecodeFile
  , showSBS
  , c8SBS
  , (!)
  ) where

import Control.Exception
import qualified Data.Binary as Binary
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Short.Internal as SBS
import qualified Data.Map.Lazy as LM
import Foreign
import Foreign.C
import GHC.Exts
import GHC.Stack
import qualified GHC.Types
import Prelude hiding (IO)

type IO a
   = HasCallStack =>
       GHC.Types.IO a

{-# INLINE marshalSBS #-}
marshalSBS :: Pool -> SBS.ShortByteString -> IO (Ptr CChar)
marshalSBS pool sbs
  | SBS.null sbs = pure nullPtr
  | otherwise = castPtr <$> pooledNewArray0 pool 0 (SBS.unpack sbs)

{-# INLINE marshalV #-}
marshalV :: (Storable a, Num n) => Pool -> [a] -> IO (Ptr a, n)
marshalV pool v
  | null v = pure (nullPtr, 0)
  | otherwise = do
    buf <- pooledNewArray pool v
    pure (buf, fromIntegral $ length v)

unI# :: Int -> Int#
unI# (I# x) = x

unIO :: GHC.Types.IO a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO (GHC.Types.IO m) = m

{-# INLINE encodeStorable #-}
encodeStorable :: Storable a => a -> SBS.ShortByteString
encodeStorable a =
  case runRW#
         (\s0 ->
            case newAlignedPinnedByteArray#
                   (unI# (sizeOf a))
                   (unI# (alignment a))
                   s0 of
              (# s1, mba #) ->
                case unsafeFreezeByteArray# mba s1 of
                  (# s2, ba #) ->
                    let addr = byteArrayContents# ba
                     in case unIO (poke (Ptr addr) a) s2 of
                          (# s3, _ #) -> (# s3, SBS.SBS ba #)) of
    (# _, r #) -> r

{-# INLINE reinterpretCast #-}
reinterpretCast :: (Storable a, Storable b) => a -> b
reinterpretCast a =
  case runRW#
         (\s0 ->
            case newPinnedByteArray# (unI# (sizeOf a)) s0 of
              (# s1, mba #) ->
                case unsafeFreezeByteArray# mba s1 of
                  (# s2, ba #) ->
                    case byteArrayContents# ba of
                      addr ->
                        case unIO (poke (Ptr addr) a) s2 of
                          (# s3, _ #) -> unIO (peek (Ptr addr)) s3) of
    (# _, r #) -> r

{-# INLINE encodeFile #-}
encodeFile :: Binary.Binary a => FilePath -> a -> IO ()
encodeFile = Binary.encodeFile

{-# INLINE decodeFile #-}
decodeFile :: Binary.Binary a => FilePath -> IO a
decodeFile = Binary.decodeFile

{-# INLINE tryDecodeFile #-}
tryDecodeFile :: Binary.Binary a => FilePath -> IO (Either SomeException a)
tryDecodeFile p = do
  r <- try $ Binary.decodeFileOrFail p
  pure $
    case r of
      Left err -> Left err
      Right (Left err) -> Left $ toException $ userError $ show err
      Right (Right v) -> Right v

{-# INLINE showSBS #-}
showSBS :: Show a => a -> SBS.ShortByteString
showSBS = fromString . show

{-# INLINE c8SBS #-}
c8SBS :: SBS.ShortByteString -> String
c8SBS = CBS.unpack . SBS.fromShort

{-# INLINE (!) #-}
(!) :: (HasCallStack, Ord k, Show k) => LM.Map k v -> k -> v
(!) m k =
  case LM.lookup k m of
    Just v -> v
    _ -> error $ show k <> " not found"
