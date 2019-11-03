{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData #-}

module Asterius.Types
  ( module Asterius.Prim,
    JSException (..),
    makeJSException,
  )
where

import Asterius.Prim
import GHC.Exception.Type
import GHC.Show
import GHC.Types

data JSException
  = JSException JSVal [Char]

instance Show JSException where
  show (JSException _ msg) = msg

instance Exception JSException

{-# INLINE makeJSException #-}
makeJSException :: JSVal -> SomeException
makeJSException v =
  toException (JSException v (fromJSString (js_err_toString v)))

foreign import javascript "${1}.stack ? ${1}.stack : ${1}.toString()"
  js_err_toString :: JSVal -> JSString
