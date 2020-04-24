module Asterius.ByteString
  ( byteStringFromJSUint8Array,
    byteStringToJSUint8Array,
    unsafeByteStringToJSUint8Array,
    lazyByteStringToJSUint8Array,
  )
where

import Asterius.Magic
import Asterius.Types
import Control.Exception
import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString, fromForeignPtr)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBS
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Int
import Foreign.Ptr

{-# INLINEABLE byteStringFromJSUint8Array #-}
byteStringFromJSUint8Array :: JSUint8Array -> ByteString
byteStringFromJSUint8Array buf =
  accursedUnutterablePerformIO $ do
    fp <- fromJSUint8Array buf
    len <- lengthOfJSUint8Array buf
    evaluate $ fromForeignPtr fp 0 len

{-# INLINEABLE byteStringToJSUint8Array #-}
byteStringToJSUint8Array :: ByteString -> JSUint8Array
byteStringToJSUint8Array bs =
  accursedUnutterablePerformIO
    $ unsafeUseAsCStringLen bs
    $ uncurry toJSUint8Array

{-# INLINEABLE unsafeByteStringToJSUint8Array #-}
unsafeByteStringToJSUint8Array :: ByteString -> JSUint8Array
unsafeByteStringToJSUint8Array bs =
  accursedUnutterablePerformIO
    $ unsafeUseAsCStringLen bs
    $ uncurry unsafeToJSUint8Array

{-# INLINEABLE lazyByteStringToJSUint8Array #-}
lazyByteStringToJSUint8Array :: LBS.ByteString -> JSUint8Array
lazyByteStringToJSUint8Array lbs =
  accursedUnutterablePerformIO $ do
    r <- js_newUint8Array $ LBS.length lbs
    let w _ LBS.Empty = pure ()
        w i (LBS.Chunk c cs) =
          unsafeUseAsCStringLen c (uncurry $ js_chunk_save r i)
            *> w (i + BS.length c) cs
     in w 0 lbs
    pure r

foreign import javascript unsafe "new Uint8Array($1)"
  js_newUint8Array ::
    Int64 -> IO JSUint8Array

foreign import javascript unsafe "$1.subarray($2).set(__asterius_jsffi.exposeMemory($3, $4))"
  js_chunk_save ::
    JSUint8Array -> Int -> Ptr a -> Int -> IO ()
