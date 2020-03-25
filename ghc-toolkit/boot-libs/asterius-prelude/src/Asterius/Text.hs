{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Asterius.Text
  ( textFromJSString,
    textToJSString,
  )
where

import Asterius.Magic
import Asterius.Types
import Data.Text.Array
import Data.Text.Internal
import GHC.Exts
import GHC.Types

{-# INLINEABLE textFromJSString #-}
textFromJSString :: JSString -> Text
textFromJSString s = accursedUnutterablePerformIO $ IO $ \s0 ->
  case lengthOfJSString s of
    len@(I# len#) -> case len# of
      0# -> (# s0, Data.Text.Internal.empty #)
      _ -> case newByteArray# (len# *# 2#) s0 of
        (# s1, mba #) -> case unsafeFreezeByteArray# mba s1 of
          (# s2, ba #) ->
            case unIO (js_loadString (byteArrayContents# ba) s) s2 of
              (# s3, () #) -> (# s3, text (Array ba) 0 len #)

{-# INLINEABLE textToJSString #-}
textToJSString :: Text -> JSString
textToJSString (Text (Array ba) (I# off#) (I# len#)) =
  js_saveString (byteArrayContents# ba `plusAddr#` (off# *# 2#)) len#

foreign import javascript unsafe "(() => {                                \
\  const buf = __asterius_jsffi.exposeMemory($1, $2.length, Uint16Array); \
\  for(let i = 0; i < $2.length; ++i) {                                   \
\    buf[i] = $2.charCodeAt(i);                                           \
\  }})()"
  js_loadString :: Addr# -> JSString -> IO ()

foreign import javascript unsafe "(new TextDecoder('utf-16le', {fatal: true})).decode(__asterius_jsffi.exposeMemory($1, $2, Uint16Array))"
  js_saveString :: Addr# -> Int# -> JSString
