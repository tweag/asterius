{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}

module Asterius.Internals
  ( encodeStorable,
    reinterpretCast,
    showBS,
    c8BS,
--     (!),
  )
where

import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Char8 as CBS
import Foreign
import GHC.Exts
import GHC.Stack
import qualified GHC.Types

unI# :: Int -> Int#
unI# (I# x) = x

unIO :: GHC.Types.IO a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO (GHC.Types.IO m) = m

{-# INLINE encodeStorable #-}
encodeStorable :: Storable a => a -> BS.ByteString
encodeStorable a = BS.unsafeCreate len $ \p -> poke (castPtr p) a
  where
    len = sizeOf a

{-# INLINE reinterpretCast #-}
reinterpretCast :: (Storable a, Storable b) => a -> b
reinterpretCast a =
  case runRW#
    ( \s0 -> case newPinnedByteArray# (unI# (sizeOf a)) s0 of
        (# s1, mba #) -> case unsafeFreezeByteArray# mba s1 of
          (# s2, ba #) -> case byteArrayContents# ba of
            addr -> case unIO (poke (Ptr addr) a) s2 of
              (# s3, _ #) -> unIO (peek (Ptr addr)) s3
    ) of
    (# _, r #) -> r

{-# INLINE showBS #-}
showBS :: Show a => a -> BS.ByteString
showBS = fromString . show

{-# INLINE c8BS #-}
c8BS :: BS.ByteString -> String
c8BS = CBS.unpack

-- {-# INLINE (!) #-}
-- (!) :: (HasCallStack, Ord k, Show k) => LM.Map k v -> k -> v
-- (!) m k = case LM.lookup k m of
--   Just v -> v
--   _ -> error $ show k <> " not found"
