{-# LANGUAGE RankNTypes #-}

module Asterius.Internals
  ( encodeStorable,
    showBS,
    c8BS,
  )
where

import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Char8 as CBS
import Foreign
import GHC.Exts

{-# INLINE encodeStorable #-}
encodeStorable :: Storable a => a -> BS.ByteString
encodeStorable a = BS.unsafeCreate len $ \p -> poke (castPtr p) a
  where
    len = sizeOf a

{-# INLINE showBS #-}
showBS :: Show a => a -> BS.ByteString
showBS = fromString . show

{-# INLINE c8BS #-}
c8BS :: BS.ByteString -> String
c8BS = CBS.unpack
