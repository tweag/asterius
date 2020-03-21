{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Asterius.Types.JSString
  ( JSString (..),
    showJSVal,
  )
where

import Asterius.Types.JSVal
import GHC.Base

newtype JSString = JSString JSVal
  deriving (Eq, Ord)

foreign import javascript unsafe "`${$1}`" showJSVal :: JSVal -> JSString
