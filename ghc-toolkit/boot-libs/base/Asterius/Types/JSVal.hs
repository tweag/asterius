{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Asterius.Types.JSVal
  ( JSVal (..),
    freeJSVal,
  )
where

import GHC.Base
import GHC.Exts

data JSVal
  = JSVal (StableName# ())

instance Eq JSVal where
  {-# INLINE (==) #-}
  (==) = js_eqJSVal

instance Ord JSVal where
  {-# INLINE (<=) #-}
  (<=) = js_leJSVal

{-# INLINE freeJSVal #-}
freeJSVal :: JSVal -> IO ()
freeJSVal (JSVal sn) = js_freeJSVal sn

foreign import javascript unsafe "__asterius_jsffi.freeJSVal($1)" js_freeJSVal :: StableName# () -> IO ()

foreign import javascript unsafe "$1 === $2" js_eqJSVal :: JSVal -> JSVal -> Bool

foreign import javascript unsafe "$1 <= $2" js_leJSVal :: JSVal -> JSVal -> Bool
