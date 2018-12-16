{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Asterius.ByteString
import Asterius.Types
import Asterius.Vault

foreign import javascript "new Date()" js_get_jsval :: IO JSVal

foreign import javascript "${1}.toString()" js_toString :: JSVal -> JSString

instance Show JSVal where
  show = show . fromJSString . js_toString

main :: IO ()
main = do
  v <- js_get_jsval
  let k = byteStringToJSArrayBuffer "key"
  vaultLookup k >>= print
  vaultInsert k v
  vaultLookup k >>= print
