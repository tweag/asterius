{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Asterius.Prim
  ( JSVal,
    JSUint8Array (..),
    JSString (..),
    JSArray (..),
    JSObject (..),
    JSFunction (..),
    lengthOfJSUint8Array,
    fromJSUint8Array,
    toJSUint8Array,
    unsafeToJSUint8Array,
    fromJSString,
    toJSString,
    fromJSArray,
    toJSArray,
    indexJSObject,
    setJSObject,
    callJSFunction,
    freeHaskellCallback,
    makeHaskellCallback,
    makeHaskellCallback1,
    makeHaskellCallback2,
    jsStringDecodeLatin1,
    jsStringEncodeLatin1,
  )
where

import Asterius.Magic
import Asterius.Types.JSArray
import Asterius.Types.JSFunction
import Asterius.Types.JSObject
import Asterius.Types.JSString
import Asterius.Types.JSUint8Array
import Asterius.Types.JSVal
import Data.String
import GHC.Magic
import GHC.Prim
import GHC.Show
import GHC.Tuple
import GHC.Types

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

foreign import javascript "__asterius_jsffi.decodeLatin1($1)"
  jsStringDecodeLatin1 :: JSUint8Array -> JSString

foreign import javascript "__asterius_jsffi.encodeLatin1($1)"
  jsStringEncodeLatin1 :: JSString -> JSUint8Array

foreign import javascript "__asterius_jsffi.makeHaskellCallback($1)"
  js_mk_hs_callback :: StablePtr# (IO ()) -> IO JSFunction

foreign import javascript "__asterius_jsffi.makeHaskellCallback1($1)"
  js_mk_hs_callback1 :: StablePtr# (JSVal -> IO ()) -> IO JSFunction

foreign import javascript "__asterius_jsffi.makeHaskellCallback2($1)"
  js_mk_hs_callback2 :: StablePtr# (JSVal -> JSVal -> IO ()) -> IO JSFunction
