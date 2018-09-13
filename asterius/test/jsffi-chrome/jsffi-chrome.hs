{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wall -ddump-to-file -ddump-rn -ddump-foreign -ddump-stg -ddump-cmm-raw -ddump-asm #-}

import Data.Foldable
import Foreign.StablePtr

foreign import javascript "console.log(${1})" js_print :: JSRef -> IO ()

foreign import javascript "\"\"" js_string_empty :: JSRef

foreign import javascript "${1}.concat(${2})" js_concat
  :: JSRef -> JSRef -> JSRef

foreign import javascript "String.fromCodePoint(${1})" js_string_fromchar
  :: Char -> JSRef

foreign import javascript "${1}.appendChild(${2})" js_appendChild
  :: JSRef -> JSRef -> IO ()

foreign import javascript "${1}.addEventListener(${2},${3})" js_addEventListener
  :: JSRef -> JSRef -> JSRef -> IO ()

foreign import javascript "__asterius_jsffi.makeHaskellCallback1(${1})" js_make_hs_callback1
  :: StablePtr (JSRef -> IO ()) -> IO JSRef

foreign import javascript "console.log(\"Veni, vidi, vici\")" js_trace :: IO ()

type JSString = JSRef

toJSString :: String -> JSString
toJSString = foldl' (\s c -> js_concat s (js_string_fromchar c)) js_string_empty

appendChild :: JSRef -> JSRef -> IO ()
appendChild = js_appendChild

makeHaskellCallback1 :: (JSRef -> IO ()) -> IO JSRef
makeHaskellCallback1 m = do
  s <- newStablePtr m
  js_make_hs_callback1 s

addEventListener :: JSRef -> String -> (JSRef -> IO ()) -> IO ()
addEventListener target event handler = do
  callback <- makeHaskellCallback1 handler
  js_addEventListener target (toJSString event) callback

data Element = Element
  { children :: [Element]
  , eventHandlers :: [(String, JSRef -> IO ())]
  }

buildElement :: Element -> IO JSRef
buildElement Element {..} = do
  e <- js_canvas_create
  for_ children $ \child -> do
    ce <- buildElement child
    appendChild e ce
  for_ eventHandlers $ uncurry $ addEventListener e
  pure e

main :: IO ()
main = do
  e <-
    buildElement
      Element {children = mempty, eventHandlers = [("click", js_print)]}
  js_body_appendChild e

foreign import javascript "document.querySelector(\"body\").appendChild(${1})" js_body_appendChild
  :: JSRef -> IO ()

foreign import javascript "document.createElement(\"canvas\")" js_canvas_create
  :: IO JSRef
