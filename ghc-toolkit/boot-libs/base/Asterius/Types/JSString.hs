{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Asterius.Types.JSString
  ( JSString (..),
  )
where

import Asterius.Magic
import Asterius.Types.JSVal
import Data.String
import GHC.Base
import GHC.Enum
import GHC.Show

newtype JSString = JSString JSVal
  deriving (Eq, Ord)

{-# INLINEABLE fromJSString #-}
fromJSString :: JSString -> String
fromJSString s = accursedUnutterablePerformIO $ do
  it <- js_fromJSString_iterator s
  let w = do
        c <- js_fromJSString_iterator_next it
        case c of
          '\0' -> do
            freeJSVal it
            pure []
          _ -> pure (pred c : accursedUnutterablePerformIO w)
   in w

instance Show JSString where
  {-# INLINE showsPrec #-}
  showsPrec p = showsPrec p . fromJSString

{-# INLINEABLE toJSString #-}
toJSString :: String -> JSString
toJSString s = accursedUnutterablePerformIO $ do
  ctx <- js_toJSString_context_new
  let w (c : cs) = js_toJSString_context_push ctx c *> w cs
      w [] = pure ()
   in w s
  r <- js_toJSString_context_result ctx
  freeJSVal ctx
  pure r

instance IsString JSString where
  {-# INLINE fromString #-}
  fromString = toJSString

foreign import javascript unsafe "$1[Symbol.iterator]()" js_fromJSString_iterator :: JSString -> IO JSVal

foreign import javascript unsafe "(() => { const r = $1.next(); return r.done ? 0 : (1 + r.value.codePointAt(0)); })()" js_fromJSString_iterator_next :: JSVal -> IO Char

foreign import javascript unsafe "['']" js_toJSString_context_new :: IO JSVal

foreign import javascript unsafe "$1[0] += String.fromCodePoint($2)" js_toJSString_context_push :: JSVal -> Char -> IO ()

foreign import javascript unsafe "$1[0]" js_toJSString_context_result :: JSVal -> IO JSString
