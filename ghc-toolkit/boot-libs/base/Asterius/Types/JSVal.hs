{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Asterius.Types.JSVal
  ( JSVal (..),
    freeJSVal,
  )
where

import GHC.Base
import GHC.Exts

-- | The lifted type representing opaque JavaScript values in the Haskell world.
-- Any other JavaScript value type must be a @newtype@ of 'JSVal'. Can be used
-- as argument/result type of JSFFI imports/exports. Managed by the garbage
-- collector.
data JSVal
  = JSVal (StableName# ())

-- | Explicitly drop a 'JSVal' reference in the runtime. Useful when writing the
-- finalizer code for a computation which involves a lot of intermediate
-- 'JSVal's.
{-# INLINE freeJSVal #-}
freeJSVal :: JSVal -> IO ()
freeJSVal (JSVal sn) = js_freeJSVal sn

foreign import javascript unsafe "__asterius_jsffi.freeJSVal($1)" js_freeJSVal :: StableName# () -> IO ()
