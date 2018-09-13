{-# OPTIONS_GHC -Wall -ddump-to-file -ddump-rn -ddump-foreign -ddump-stg -ddump-cmm-raw -ddump-asm #-}

module WebAPI
  ( consoleLog
  , createElement
  , setAttribute
  , appendChild
  , setHidden
  , addEventListener
  , createTextNode
  , replaceWith
  ) where

import AsteriusPrim

{-# INLINEABLE consoleLog #-}
consoleLog :: JSRef -> IO ()
consoleLog = js_console_log

{-# INLINEABLE createElement #-}
createElement :: String -> IO JSRef
createElement = js_createElement . toJSString

{-# INLINEABLE setAttribute #-}
setAttribute :: JSRef -> String -> String -> IO ()
setAttribute e k v = js_setAttribute e (toJSString k) (toJSString v)

{-# INLINEABLE appendChild #-}
appendChild :: JSRef -> JSRef -> IO ()
appendChild = js_appendChild

{-# INLINEABLE setHidden #-}
setHidden :: JSRef -> Bool -> IO ()
setHidden = js_element_set_hidden

{-# INLINEABLE addEventListener #-}
addEventListener :: JSRef -> String -> (JSRef -> IO ()) -> IO ()
addEventListener target event handler = do
  callback <- makeHaskellCallback1 handler
  js_addEventListener target (toJSString event) callback

{-# INLINEABLE createTextNode #-}
createTextNode :: String -> IO JSRef
createTextNode = js_createTextNode . toJSString

{-# INLINEABLE replaceWith #-}
replaceWith :: JSRef -> JSRef -> IO ()
replaceWith = js_replaceWith

foreign import javascript "console.log(${1})" js_console_log :: JSRef -> IO ()

foreign import javascript "document.createElement(${1})" js_createElement
  :: JSRef -> IO JSRef

foreign import javascript "${1}.setAttribute(${2},${3})" js_setAttribute
  :: JSRef -> JSRef -> JSRef -> IO ()

foreign import javascript "${1}.appendChild(${2})" js_appendChild
  :: JSRef -> JSRef -> IO ()

foreign import javascript "${1}.hidden = ${2}" js_element_set_hidden
  :: JSRef -> Bool -> IO ()

foreign import javascript "${1}.addEventListener(${2},${3})" js_addEventListener
  :: JSRef -> JSRef -> JSRef -> IO ()

foreign import javascript "document.createTextNode(${1})" js_createTextNode
  :: JSRef -> IO JSRef

foreign import javascript "${1}.replaceWith(${2})" js_replaceWith :: JSRef -> JSRef -> IO ()
