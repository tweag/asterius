{-# LANGUAGE NoImplicitPrelude #-}

module Asterius.Types.JSException
  ( JSException (..),
    mkJSException,
  )
where

import Asterius.Types.JSString
import Asterius.Types.JSVal
import GHC.Base
import GHC.Exception.Type
import GHC.Show

data JSException
  = JSException !JSVal String

instance Show JSException where
  show (JSException _ msg) = "JSException " <> show msg

instance Exception JSException

mkJSException :: JSVal -> SomeException
mkJSException v =
  toException (JSException v (fromJSString (js_show_err v)))

foreign import javascript "$1.stack ? $1.stack : `${$1}`"
  js_show_err :: JSVal -> JSString
