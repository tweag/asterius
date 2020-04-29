{-# LANGUAGE InterruptibleFFI #-}

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
import Data.ByteString.Unsafe
import Data.Coerce
import Foreign.Ptr

createElement :: String -> IO JSVal
createElement = js_createElement . toJSString

setAttribute :: JSVal -> String -> String -> IO ()
setAttribute e k v = js_setAttribute e (toJSString k) (toJSString v)

addEventListener :: JSVal -> String -> (JSObject -> IO ()) -> IO ()
addEventListener target event handler = do
  callback <- makeHaskellCallback1 handler
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
localStorageSetItem k v = do
  let ks = toJSString k
  vs <- unsafeUseAsCStringLen v $ uncurry js_encode
  js_localStorage_setItem ks vs
  freeJSVal (coerce ks)
  freeJSVal (coerce vs)

localStorageGetItem :: String -> IO (Maybe BS.ByteString)
localStorageGetItem k = do
  let ks = toJSString k
  f <- js_localStorage_hasItem ks
  r <-
    if f
      then do
        vs <- js_localStorage_getItem ks
        buf <- js_decode vs
        let r = Just $ byteStringFromJSUint8Array buf
        freeJSVal (coerce vs)
        freeJSVal (coerce buf)
        pure r
      else pure Nothing
  freeJSVal (coerce ks)
  pure r

foreign import javascript "console.log($1)" consoleLog :: JSVal -> IO ()

foreign import javascript "document.createElement($1)"
  js_createElement :: JSString -> IO JSVal

foreign import javascript "$1.setAttribute($2,$3)"
  js_setAttribute :: JSVal -> JSString -> JSString -> IO ()

foreign import javascript "$1.appendChild($2)"
  appendChild :: JSVal -> JSVal -> IO ()

foreign import javascript "$1.hidden = $2"
  setHidden :: JSVal -> Bool -> IO ()

foreign import javascript "$1.addEventListener($2,$3)"
  js_addEventListener :: JSVal -> JSString -> JSFunction -> IO ()

foreign import javascript "document.createTextNode($1)"
  js_createTextNode :: JSString -> IO JSVal

foreign import javascript "$1.replaceWith($2)"
  replaceWith :: JSVal -> JSVal -> IO ()

foreign import javascript "window.addEventListener(\"popstate\", $1)"
  onPopstate :: JSFunction -> IO ()

foreign import javascript "window.location.href.split(\"#/\")[1] || \"\""
  js_url_mode :: IO JSString

foreign import javascript "Math.random().toString(36).slice(2)"
  js_randomString :: IO JSString

foreign import javascript "document.getElementById($1)"
  js_getElementById :: JSString -> IO JSVal

foreign import javascript "localStorage.setItem($1,$2)"
  js_localStorage_setItem :: JSString -> JSString -> IO ()

foreign import javascript "localStorage.getItem($1) !== null"
  js_localStorage_hasItem :: JSString -> IO Bool

foreign import javascript "localStorage.getItem($1)"
  js_localStorage_getItem :: JSString -> IO JSString

foreign import javascript interruptible "new Promise((resolve, reject) => { \
\  const buf = __asterius_jsffi.exposeMemory($1,$2);                        \
\  const blob = new Blob(buf);                                              \
\  const r = new FileReader();                                              \
\  r.addEventListener('load', () => { resolve(r.result); });                \
\  r.readAsDataURL(blob);                                                   \
\  })"
  js_encode :: Ptr a -> Int -> IO JSString

foreign import javascript interruptible "fetch($1).then(b => b.arrayBuffer()).then(b => new Uint8Array(b))"
  js_decode :: JSString -> IO JSUint8Array

foreign import javascript "wrapper"
  makeHaskellCallback1 :: (JSObject -> IO ()) -> IO JSFunction
