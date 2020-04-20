module Asterius.ByteString
  ( byteStringFromJSUint8Array,
    byteStringToJSUint8Array,
    unsafeByteStringToJSUint8Array,
  )
where

import Asterius.Magic
import Asterius.Types
import Control.Exception
import Data.ByteString.Internal
  ( ByteString,
    fromForeignPtr,
  )
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)

{-# INLINEABLE byteStringFromJSUint8Array #-}
byteStringFromJSUint8Array :: JSUint8Array -> ByteString
byteStringFromJSUint8Array buf = accursedUnutterablePerformIO $ do
  fp <- fromJSUint8Array buf
  len <- lengthOfJSUint8Array buf
  evaluate $ fromForeignPtr fp 0 len

{-# INLINEABLE byteStringToJSUint8Array #-}
byteStringToJSUint8Array :: ByteString -> JSUint8Array
byteStringToJSUint8Array bs =
  accursedUnutterablePerformIO $ unsafeUseAsCStringLen bs $
    uncurry
      toJSUint8Array

{-# INLINEABLE unsafeByteStringToJSUint8Array #-}
unsafeByteStringToJSUint8Array :: ByteString -> JSUint8Array
unsafeByteStringToJSUint8Array bs =
  accursedUnutterablePerformIO $ unsafeUseAsCStringLen bs $
    uncurry
      unsafeToJSUint8Array
