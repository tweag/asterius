module Asterius.Aeson
  ( jsonToJSVal,
    jsonFromJSVal,
  )
where

import Asterius.Types
import Asterius.UTF8
import qualified Data.Aeson as A

{-# INLINE jsonToJSVal #-}
jsonToJSVal :: A.ToJSON a => a -> JSVal
jsonToJSVal = js_dec . utf8ToJSString . A.encode

{-# INLINE jsonFromJSVal #-}
jsonFromJSVal :: A.FromJSON a => JSVal -> Either String a
jsonFromJSVal = A.eitherDecode' . utf8FromJSString . js_enc

foreign import javascript "JSON.stringify(${1})" js_enc :: JSVal -> JSString

foreign import javascript "JSON.parse(${1})" js_dec :: JSString -> JSVal
