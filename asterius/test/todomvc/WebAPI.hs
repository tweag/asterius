module WebAPI
  ( consoleLog,
    createElement,
    setAttribute,
    appendChild,
    setHidden,
    addEventListener,
    createTextNode,
    replaceWith,
    onPopstate,
    getURLMode,
    randomString,
    getElementById,
    localStorageSetItem,
    localStorageGetItem,
  )
where

import Asterius.ByteString
import Asterius.Types
import qualified Data.ByteString as BS
import Data.Coerce

createElement :: String -> IO JSVal
createElement = js_createElement . toJSString

setAttribute :: JSVal -> String -> String -> IO ()
setAttribute e k v = js_setAttribute e (toJSString k) (toJSString v)

addEventListener :: JSVal -> String -> (JSObject -> IO ()) -> IO ()
addEventListener target event handler = do
  callback <- makeHaskellCallback1 $ coerce handler
  js_addEventListener target (toJSString event) callback

createTextNode :: String -> IO JSVal
createTextNode = js_createTextNode . toJSString

getURLMode :: IO String
getURLMode = fromJSString <$> js_url_mode

randomString :: IO String
randomString = fromJSString <$> js_randomString

getElementById :: String -> IO JSVal
getElementById k = js_getElementById (toJSString k)

localStorageSetItem :: String -> BS.ByteString -> IO ()
localStorageSetItem k v =
  js_localStorage_setItem
    (toJSString k)
    (jsStringDecodeLatin1 (byteStringToJSArrayBuffer v))

localStorageGetItem :: String -> IO (Maybe BS.ByteString)
localStorageGetItem k = do
  f <- js_localStorage_hasItem js_k
  if f
    then
      Just
        . byteStringFromJSArrayBuffer
        . jsStringEncodeLatin1
        <$> js_localStorage_getItem js_k
    else pure Nothing
  where
    js_k = toJSString k

foreign import javascript "console.log(${1})" consoleLog :: JSVal -> IO ()

foreign import javascript "document.createElement(${1})"
  js_createElement :: JSString -> IO JSVal

foreign import javascript "${1}.setAttribute(${2},${3})"
  js_setAttribute :: JSVal -> JSString -> JSString -> IO ()

foreign import javascript "${1}.appendChild(${2})"
  appendChild :: JSVal -> JSVal -> IO ()

foreign import javascript "${1}.hidden = ${2}"
  setHidden :: JSVal -> Bool -> IO ()

foreign import javascript "${1}.addEventListener(${2},${3})"
  js_addEventListener :: JSVal -> JSString -> JSFunction -> IO ()

foreign import javascript "document.createTextNode(${1})"
  js_createTextNode :: JSString -> IO JSVal

foreign import javascript "${1}.replaceWith(${2})"
  replaceWith :: JSVal -> JSVal -> IO ()

foreign import javascript "window.addEventListener(\"popstate\", ${1})"
  onPopstate :: JSFunction -> IO ()

foreign import javascript "window.location.href.split(\"#/\")[1] || \"\""
  js_url_mode :: IO JSString

foreign import javascript "Math.random().toString(36).slice(2)"
  js_randomString :: IO JSString

foreign import javascript "document.getElementById(${1})"
  js_getElementById :: JSString -> IO JSVal

foreign import javascript "localStorage.setItem(${1},${2})"
  js_localStorage_setItem :: JSString -> JSString -> IO ()

foreign import javascript "localStorage.getItem(${1}) !== null"
  js_localStorage_hasItem :: JSString -> IO Bool

foreign import javascript "localStorage.getItem(${1})"
  js_localStorage_getItem :: JSString -> IO JSString
