module Asterius.ByteString
  ( byteStringFromJSUint8Array,
    byteStringToJSUint8Array,
    unsafeByteStringToJSUint8Array,
  )
where

import Asterius.Types
import Control.Exception
import Data.ByteString.Internal
  ( ByteString,
    fromForeignPtr,
  )
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)

{-# INLINEABLE byteStringFromJSUint8Array #-}
byteStringFromJSUint8Array :: JSUint8Array -> IO ByteString
byteStringFromJSUint8Array buf = do
  fp <- fromJSUint8Array buf
  len <- lengthOfJSUint8Array buf
  evaluate $ fromForeignPtr fp 0 len

{-# INLINEABLE byteStringToJSUint8Array #-}
byteStringToJSUint8Array :: ByteString -> IO JSUint8Array
byteStringToJSUint8Array bs = unsafeUseAsCStringLen bs $ uncurry toJSUint8Array

{-# INLINEABLE unsafeByteStringToJSUint8Array #-}
unsafeByteStringToJSUint8Array :: ByteString -> IO JSUint8Array
unsafeByteStringToJSUint8Array bs =
  unsafeUseAsCStringLen bs $ uncurry unsafeToJSUint8Array
