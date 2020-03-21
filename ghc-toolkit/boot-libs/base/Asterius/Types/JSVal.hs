{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Asterius.Types.JSVal
  ( JSVal (..),
    freeJSVal,
    setJSVal,
    moveJSVal,
  )
where

import GHC.Base
import GHC.Exts

type JSVal# = StableName# ()

data JSVal
  = JSVal JSVal#

instance Eq JSVal where
  {-# INLINE (==) #-}
  (==) = js_eqJSVal

instance Ord JSVal where
  {-# INLINE (<=) #-}
  (<=) = js_leJSVal

{-# INLINE freeJSVal #-}
freeJSVal :: JSVal -> IO ()
freeJSVal (JSVal sn) = js_freeJSVal sn

{-# INLINE setJSVal #-}
setJSVal :: JSVal -> JSVal -> IO ()
setJSVal (JSVal l) (JSVal r) = js_setJSVal l r

{-# INLINE moveJSVal #-}
moveJSVal :: JSVal -> JSVal -> IO ()
moveJSVal (JSVal l) (JSVal r) = js_moveJSVal l r

foreign import javascript unsafe "__asterius_jsffi.freeJSVal($1)" js_freeJSVal :: JSVal# -> IO ()

foreign import javascript unsafe "__asterius_jsffi.setJSVal($1, $2)" js_setJSVal :: JSVal# -> JSVal# -> IO ()

foreign import javascript unsafe "__asterius_jsffi.moveJSVal($1, $2)" js_moveJSVal :: JSVal# -> JSVal# -> IO ()

foreign import javascript unsafe "$1 === $2" js_eqJSVal :: JSVal -> JSVal -> Bool

foreign import javascript unsafe "$1 <= $2" js_leJSVal :: JSVal -> JSVal -> Bool
