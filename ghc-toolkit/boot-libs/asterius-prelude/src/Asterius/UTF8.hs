module Asterius.UTF8
  ( utf8ToJSString,
    utf8FromJSString,
  )
where

import Asterius.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Internal as LBS
import qualified Data.ByteString.Unsafe as BS
import Foreign

{-# INLINEABLE utf8ToJSString #-}
utf8ToJSString :: LBS.ByteString -> IO JSString
utf8ToJSString LBS.Empty = pure js_str_empty
utf8ToJSString s = do
  dec <- js_dec
  let w (LBS.Chunk c cs) = BS.unsafeUseAsCStringLen c (uncurry (js_dec_chunk dec)) *> w cs
      w LBS.Empty = pure ()
   in w s
  r <- js_dec_result dec
  freeJSVal dec
  pure r

{-# INLINEABLE utf8FromJSString #-}
utf8FromJSString :: JSString -> IO BS.ByteString
utf8FromJSString s = do
  let l = lengthOfJSString s
  case l of
    0 -> pure mempty
    _ -> do
      p <- mallocBytes (l * 3)
      l' <- js_utf8_from_str s p (l * 3)
      BS.unsafePackCStringLen (p, l')

foreign import javascript unsafe "''"
  js_str_empty :: JSString

foreign import javascript unsafe "(() => {              \
\  const dec = new TextDecoder('utf-8', {fatal: true}); \
\  dec.result = '';                                     \
\  return dec;                                          \
\  })()"
  js_dec :: IO JSVal

foreign import javascript unsafe "$1.result += $1.decode(__asterius_jsffi.exposeMemory($2, $3), {stream: true})"
  js_dec_chunk :: JSVal -> Ptr a -> Int -> IO ()

foreign import javascript unsafe "$1.result"
  js_dec_result :: JSVal -> IO JSString

foreign import javascript unsafe "(new TextEncoder()).encodeInto($1, __asterius_jsffi.exposeMemory($2, $3)).written"
  js_utf8_from_str :: JSString -> Ptr a -> Int -> IO Int
