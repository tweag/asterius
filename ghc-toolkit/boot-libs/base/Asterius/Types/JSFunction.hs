{-# LANGUAGE NoImplicitPrelude #-}

module Asterius.Types.JSFunction
  ( JSFunction (..),
    callJSFunction,
    freeHaskellCallback,
  )
where

import Asterius.Types.JSArray
import Asterius.Types.JSString ()
import Asterius.Types.JSVal
import GHC.Base
import GHC.Show

newtype JSFunction
  = JSFunction JSVal
  deriving (Show)

{-# INLINE callJSFunction #-}
callJSFunction :: JSFunction -> [JSVal] -> IO JSVal
callJSFunction f args = js_apply f (toJSArray args)

foreign import javascript unsafe "$1(...$2)"
  js_apply :: JSFunction -> JSArray -> IO JSVal

foreign import ccall unsafe "freeHaskellCallback"
  freeHaskellCallback :: JSFunction -> IO ()
