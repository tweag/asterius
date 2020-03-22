{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Asterius.Prim
  ( JSVal,
    JSArrayBuffer (..),
    JSString (..),
    JSArray (..),
    JSObject (..),
    JSFunction (..),
    fromJSArrayBuffer,
    toJSArrayBuffer,
    fromJSString,
    toJSString,
    fromJSArray,
    toJSArray,
    indexJSObject,
    setJSObject,
    jsonParse,
    jsonStringify,
    callJSFunction,
    makeHaskellCallback,
    makeHaskellCallback1,
    makeHaskellCallback2,
    jsStringDecodeUTF8,
    jsStringEncodeUTF8,
    jsStringDecodeLatin1,
    jsStringEncodeLatin1,
    jsStringDecodeUTF16LE,
    jsStringEncodeUTF16LE,
    jsStringDecodeUTF32LE,
    jsStringEncodeUTF32LE,
  )
where

import Asterius.Magic
import Asterius.Types.JSArray
import Asterius.Types.JSObject
import Asterius.Types.JSString
import Asterius.Types.JSVal
import Data.String
import GHC.Magic
import GHC.Prim
import GHC.Show
import GHC.Tuple
import GHC.Types

newtype JSArrayBuffer
  = JSArrayBuffer JSVal

newtype JSFunction
  = JSFunction JSVal

{-# INLINE fromJSArrayBuffer #-}
fromJSArrayBuffer :: JSArrayBuffer -> ByteArray#
fromJSArrayBuffer buf = unsafeCoerce# (c_fromJSArrayBuffer buf)

{-# INLINE toJSArrayBuffer #-}
toJSArrayBuffer :: Addr# -> Int -> JSArrayBuffer
toJSArrayBuffer = c_toJSArrayBuffer

{-# INLINE jsonParse #-}
jsonParse :: [Char] -> JSVal
jsonParse s = js_jsonParse (toJSString s)

{-# INLINE jsonStringify #-}
jsonStringify :: JSVal -> [Char]
jsonStringify v = fromJSString (js_jsonStringify v)

{-# INLINE callJSFunction #-}
callJSFunction :: JSFunction -> [JSVal] -> IO JSVal
callJSFunction f args = js_apply f (toJSArray args)

{-# INLINE makeHaskellCallback #-}
makeHaskellCallback :: IO () -> IO JSFunction
makeHaskellCallback f =
  IO
    ( \s0 -> case makeStablePtr# f s0 of
        (# s1, sp #) -> unIO (js_mk_hs_callback sp) s1
    )

{-# INLINE makeHaskellCallback1 #-}
makeHaskellCallback1 :: (JSVal -> IO ()) -> IO JSFunction
makeHaskellCallback1 f =
  IO
    ( \s0 -> case makeStablePtr# f s0 of
        (# s1, sp #) -> unIO (js_mk_hs_callback1 sp) s1
    )

{-# INLINE makeHaskellCallback2 #-}
makeHaskellCallback2 :: (JSVal -> JSVal -> IO ()) -> IO JSFunction
makeHaskellCallback2 f =
  IO
    ( \s0 -> case makeStablePtr# f s0 of
        (# s1, sp #) -> unIO (js_mk_hs_callback2 sp) s1
    )

foreign import ccall unsafe "__asterius_fromJSArrayBuffer"
  c_fromJSArrayBuffer :: JSArrayBuffer -> Any

foreign import ccall unsafe "__asterius_toJSArrayBuffer"
  c_toJSArrayBuffer :: Addr# -> Int -> JSArrayBuffer

foreign import javascript "__asterius_jsffi.decodeUTF8($1)"
  jsStringDecodeUTF8 :: JSArrayBuffer -> JSString

foreign import javascript "__asterius_jsffi.encodeUTF8($1)"
  jsStringEncodeUTF8 :: JSString -> JSArrayBuffer

foreign import javascript "__asterius_jsffi.decodeLatin1($1)"
  jsStringDecodeLatin1 :: JSArrayBuffer -> JSString

foreign import javascript "__asterius_jsffi.encodeLatin1($1)"
  jsStringEncodeLatin1 :: JSString -> JSArrayBuffer

foreign import javascript "__asterius_jsffi.decodeUTF16LE($1)"
  jsStringDecodeUTF16LE :: JSArrayBuffer -> JSString

foreign import javascript "__asterius_jsffi.encodeUTF16LE($1)"
  jsStringEncodeUTF16LE :: JSString -> JSArrayBuffer

foreign import javascript "__asterius_jsffi.decodeUTF32LE($1)"
  jsStringDecodeUTF32LE :: JSArrayBuffer -> JSString

foreign import javascript "__asterius_jsffi.encodeUTF32LE($1)"
  jsStringEncodeUTF32LE :: JSString -> JSArrayBuffer

foreign import javascript "$1.apply({},$2)"
  js_apply :: JSFunction -> JSArray -> IO JSVal

foreign import javascript "__asterius_jsffi.makeHaskellCallback($1)"
  js_mk_hs_callback :: StablePtr# (IO ()) -> IO JSFunction

foreign import javascript "__asterius_jsffi.makeHaskellCallback1($1)"
  js_mk_hs_callback1 :: StablePtr# (JSVal -> IO ()) -> IO JSFunction

foreign import javascript "__asterius_jsffi.makeHaskellCallback2($1)"
  js_mk_hs_callback2 :: StablePtr# (JSVal -> JSVal -> IO ()) -> IO JSFunction

foreign import javascript "JSON.parse($1)" js_jsonParse :: JSString -> JSVal

foreign import javascript "JSON.stringify($1)"
  js_jsonStringify :: JSVal -> JSString
