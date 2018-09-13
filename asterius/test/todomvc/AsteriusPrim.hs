{-# OPTIONS_GHC -Wall -ddump-to-file -ddump-rn -ddump-foreign -ddump-stg -ddump-cmm-raw -ddump-asm #-}

module AsteriusPrim
  ( JSString
  , emptyJSString
  , concatJSString
  , indexJSString
  , toJSString
  , fromJSString
  , JSArray
  , emptyJSArray
  , concatJSArray
  , indexJSArray
  , toJSArray
  , fromJSArray
  , JSObject
  , emptyJSObject
  , indexJSObject
  , callJSObjectMethod
  , console
  , document
  , json
  , window
  , JSFunction
  , callJSFunction
  , makeHaskellCallback
  , makeHaskellCallback1
  , randomId
  , trace
  ) where

import Data.List
import Foreign.StablePtr

type JSString = JSRef

{-# INLINEABLE emptyJSString #-}
emptyJSString :: JSString
emptyJSString = js_string_empty

{-# INLINEABLE concatJSString #-}
concatJSString :: JSString -> JSString -> JSString
concatJSString = js_concat

{-# INLINEABLE indexJSString #-}
indexJSString :: JSString -> Int -> Char
indexJSString = js_string_tochar

{-# INLINEABLE toJSString #-}
toJSString :: String -> JSString
toJSString = foldl' (\s c -> js_concat s (js_string_fromchar c)) js_string_empty

{-# INLINEABLE fromJSString #-}
fromJSString :: JSString -> String
fromJSString s = [js_string_tochar s i | i <- [0 .. js_length s - 1]]

type JSArray = JSRef

{-# INLINEABLE emptyJSArray #-}
emptyJSArray :: JSArray
emptyJSArray = js_array_empty

{-# INLINEABLE concatJSArray #-}
concatJSArray :: JSArray -> JSArray -> JSArray
concatJSArray = js_concat

{-# INLINEABLE indexJSArray #-}
indexJSArray :: JSArray -> Int -> JSRef
indexJSArray = js_index_by_int

{-# INLINEABLE toJSArray #-}
toJSArray :: [JSRef] -> JSArray
toJSArray = foldl' js_concat js_array_empty

{-# INLINEABLE fromJSArray #-}
fromJSArray :: JSArray -> [JSRef]
fromJSArray arr = [js_index_by_int arr i | i <- [0 .. js_length arr - 1]]

type JSObject = JSRef

{-# INLINEABLE emptyJSObject #-}
emptyJSObject :: JSObject
emptyJSObject = js_object_empty

{-# INLINEABLE indexJSObject #-}
indexJSObject :: JSObject -> String -> JSRef
indexJSObject obj k = js_index_by_jsref obj (toJSString k)

{-# INLINEABLE callJSObjectMethod #-}
callJSObjectMethod :: JSObject -> String -> [JSRef] -> IO JSRef
callJSObjectMethod obj f args =
  js_function_apply (js_index_by_jsref obj (toJSString f)) obj (toJSArray args)

{-# INLINEABLE console #-}
console :: JSObject
console = js_console

{-# INLINEABLE document #-}
document :: JSObject
document = js_document

{-# INLINEABLE json #-}
json :: JSObject
json = js_json

{-# INLINEABLE window #-}
window :: JSObject
window = js_window

type JSFunction = JSRef

{-# INLINEABLE callJSFunction #-}
callJSFunction :: JSFunction -> [JSRef] -> IO JSRef
callJSFunction f args = js_function_apply f js_object_empty (toJSArray args)

{-# INLINEABLE makeHaskellCallback #-}
makeHaskellCallback :: IO () -> IO JSRef
makeHaskellCallback m = do
  s <- newStablePtr m
  js_make_hs_callback s

{-# INLINEABLE makeHaskellCallback1 #-}
makeHaskellCallback1 :: (JSRef -> IO ()) -> IO JSRef
makeHaskellCallback1 m = do
  s <- newStablePtr m
  js_make_hs_callback1 s

{-# INLINEABLE randomId #-}
randomId :: IO String
randomId = fromJSString <$> js_random_id

foreign import javascript "\"\"" js_string_empty :: JSRef

foreign import javascript "${1}.concat(${2})" js_concat
  :: JSRef -> JSRef -> JSRef

foreign import javascript "${1}.length" js_length :: JSRef -> Int

foreign import javascript "String.fromCodePoint(${1})" js_string_fromchar
  :: Char -> JSRef

foreign import javascript "${1}.codePointAt(${2})" js_string_tochar
  :: JSRef -> Int -> Char

foreign import javascript "[]" js_array_empty :: JSRef

foreign import javascript "${1}[${2}]" js_index_by_int :: JSRef -> Int -> JSRef

foreign import javascript "{}" js_object_empty :: JSRef

foreign import javascript "${1}[${2}]" js_index_by_jsref
  :: JSRef -> JSRef -> JSRef

foreign import javascript "console" js_console :: JSRef

foreign import javascript "document" js_document :: JSRef

foreign import javascript "JSON" js_json :: JSRef

foreign import javascript "window" js_window :: JSRef

foreign import javascript "${1}.apply(${2}, ${3})" js_function_apply
  :: JSRef -> JSRef -> JSRef -> IO JSRef

foreign import javascript "__asterius_jsffi.makeHaskellCallback(${1})" js_make_hs_callback
  :: StablePtr (IO ()) -> IO JSRef

foreign import javascript "__asterius_jsffi.makeHaskellCallback1(${1})" js_make_hs_callback1
  :: StablePtr (JSRef -> IO ()) -> IO JSRef

foreign import javascript "Math.random().toString(36).slice(2)" js_random_id :: IO JSRef

foreign import javascript "console.log(\"Veni, vidi, vici\")" trace :: IO ()
