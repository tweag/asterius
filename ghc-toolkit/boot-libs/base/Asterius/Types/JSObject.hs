{-# LANGUAGE NoImplicitPrelude #-}

module Asterius.Types.JSObject
  ( JSObject (..),
    indexJSObject,
    setJSObject,
  )
where

import Asterius.Types.JSString
import Asterius.Types.JSVal
import GHC.Base

newtype JSObject
  = JSObject JSVal

{-# INLINE indexJSObject #-}
indexJSObject :: JSObject -> String -> IO JSVal
indexJSObject obj k = js_object_index obj (toJSString k)

{-# INLINE setJSObject #-}
setJSObject :: JSObject -> String -> JSVal -> IO ()
setJSObject obj k = js_object_set obj (toJSString k)

foreign import javascript unsafe "$1[$2]"
  js_object_index :: JSObject -> JSString -> IO JSVal

foreign import javascript unsafe "$1[$2] = $3"
  js_object_set :: JSObject -> JSString -> JSVal -> IO ()
