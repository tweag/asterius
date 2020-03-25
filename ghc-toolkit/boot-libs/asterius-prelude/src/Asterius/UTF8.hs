module Asterius.UTF8
  ( utf8ToJSString,
    utf8FromJSString,
  )
where

import Asterius.Magic
import Asterius.Types
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as BS
import Foreign

{-# INLINE utf8ToJSString #-}
utf8ToJSString :: LBS.ByteString -> JSString
utf8ToJSString s
  | LBS.null s = js_str_empty
  | otherwise = accursedUnutterablePerformIO $ do
    dec <- js_dec
    LBS.foldrChunks
      (\c m -> BS.unsafeUseAsCStringLen c (uncurry (js_dec_chunk dec)) *> m)
      (pure ())
      s
    js_dec_result dec

{-# INLINE utf8FromJSString #-}
utf8FromJSString :: JSString -> LBS.ByteString
utf8FromJSString s = accursedUnutterablePerformIO $ do
  l <- js_str_len s
  if l == 0
    then pure mempty
    else fmap LBS.fromStrict $ BS.createUptoN (l * 3) $ \p ->
      js_utf8_from_str s p (l * 3)

foreign import javascript "''" js_str_empty :: JSString

foreign import javascript "(() => {const dec = new TextDecoder('utf-8', {fatal: true}); dec.result = ''; return dec;})()"
  js_dec :: IO JSVal

foreign import javascript "$1.result += $1.decode(new Uint8Array(__asterius_jsffi.exports.memory.buffer, $2 & 0xffffffff, $3), {stream: true})"
  js_dec_chunk :: JSVal -> Ptr a -> Int -> IO ()

foreign import javascript "$1.result" js_dec_result :: JSVal -> IO JSString

foreign import javascript "$1.length" js_str_len :: JSString -> IO Int

foreign import javascript "(new TextEncoder()).encodeInto($1, new Uint8Array(__asterius_jsffi.exports.memory.buffer, $2 & 0xffffffff, $3)).written"
  js_utf8_from_str :: JSString -> Ptr a -> Int -> IO Int
