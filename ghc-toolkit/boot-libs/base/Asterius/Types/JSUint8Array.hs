{-# LANGUAGE NoImplicitPrelude #-}

module Asterius.Types.JSUint8Array
  ( JSUint8Array (..),
    lengthOfJSUint8Array,
    fromJSUint8Array,
    toJSUint8Array,
    unsafeToJSUint8Array,
  )
where

import Asterius.Types.JSString ()
import Asterius.Types.JSVal
import Foreign.ForeignPtr
import GHC.Base
import GHC.ForeignPtr
import GHC.Ptr
import GHC.Show

newtype JSUint8Array = JSUint8Array JSVal deriving (Show)

{-# INLINEABLE fromJSUint8Array #-}
fromJSUint8Array :: JSUint8Array -> IO (ForeignPtr a)
fromJSUint8Array src = do
  len <- lengthOfJSUint8Array src
  r <- mallocPlainForeignPtrBytes len
  withForeignPtr r $ \p -> js_loadUint8Array p len src
  pure r

foreign import javascript unsafe "$1.length"
  lengthOfJSUint8Array :: JSUint8Array -> IO Int

foreign import javascript unsafe "__asterius_jsffi.exposeMemory($1,$2)"
  unsafeToJSUint8Array :: Ptr a -> Int -> IO JSUint8Array

foreign import javascript unsafe "new Uint8Array(__asterius_jsffi.exposeMemory($1,$2))"
  toJSUint8Array :: Ptr a -> Int -> IO JSUint8Array

foreign import javascript unsafe "__asterius_jsffi.exposeMemory($1,$2).set($3)"
  js_loadUint8Array :: Ptr a -> Int -> JSUint8Array -> IO ()
