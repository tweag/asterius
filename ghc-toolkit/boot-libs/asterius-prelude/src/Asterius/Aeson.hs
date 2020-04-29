module Asterius.Aeson
  ( jsonToJSVal,
    jsonFromJSVal,
    jsonFromJSVal',
  )
where

import Asterius.Magic
import Asterius.Types
import Asterius.UTF8
import qualified Data.Aeson as A
import Data.Coerce

{-# INLINE jsonToJSVal #-}
jsonToJSVal :: A.ToJSON a => a -> JSVal
jsonToJSVal v = accursedUnutterablePerformIO $ do
  s <- utf8ToJSString $ A.encode v
  r <- js_dec s
  freeJSVal (coerce s)
  pure r

{-# INLINE jsonFromJSVal #-}
jsonFromJSVal :: A.FromJSON a => JSVal -> Either String a
jsonFromJSVal v = accursedUnutterablePerformIO $ do
  s <- js_enc v
  bs <- utf8FromJSString s
  freeJSVal (coerce s)
  pure $ A.eitherDecodeStrict' bs

{-# INLINE jsonFromJSVal' #-}
jsonFromJSVal' :: A.FromJSON a => JSVal -> a
jsonFromJSVal' v = case jsonFromJSVal v of
  Left e -> error e
  Right r -> r

foreign import javascript unsafe "JSON.stringify($1)"
  js_enc :: JSVal -> IO JSString

foreign import javascript unsafe "JSON.parse($1)"
  js_dec :: JSString -> IO JSVal
