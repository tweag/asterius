{-# OPTIONS_GHC -Wall -ddump-to-file -ddump-rn -ddump-foreign -ddump-stg -ddump-cmm-raw -ddump-asm #-}

module AsteriusPrim
  ( emptyJSString,
    concatJSString,
    indexJSString,
    toJSString,
    fromJSString,
    emptyJSArray,
    concatJSArray,
    indexJSArray,
    toJSArray,
    fromJSArray,
    emptyJSObject,
    indexJSObject,
    callJSObjectMethod,
    json,
    callJSFunction,
  )
where

import Asterius.Types (JSVal (..))
import Data.List

{-# INLINEABLE emptyJSString #-}
emptyJSString :: JSVal
emptyJSString = js_string_empty

{-# INLINEABLE concatJSString #-}
concatJSString :: JSVal -> JSVal -> JSVal
concatJSString = js_concat

{-# INLINEABLE indexJSString #-}
indexJSString :: JSVal -> Int -> Char
indexJSString = js_string_tochar

{-# INLINEABLE toJSString #-}
toJSString :: String -> JSVal
toJSString =
  foldl' (\s c -> js_concat s (js_string_fromchar c)) js_string_empty

{-# INLINEABLE fromJSString #-}
fromJSString :: JSVal -> String
fromJSString s = [js_string_tochar s i | i <- [0 .. js_length s - 1]]

{-# INLINEABLE emptyJSArray #-}
emptyJSArray :: JSVal
emptyJSArray = js_array_empty

{-# INLINEABLE concatJSArray #-}
concatJSArray :: JSVal -> JSVal -> JSVal
concatJSArray = js_concat

{-# INLINEABLE indexJSArray #-}
indexJSArray :: JSVal -> Int -> JSVal
indexJSArray = js_index_by_int

{-# INLINEABLE toJSArray #-}
toJSArray :: [JSVal] -> JSVal
toJSArray = foldl' js_concat js_array_empty

{-# INLINEABLE fromJSArray #-}
fromJSArray :: JSVal -> [JSVal]
fromJSArray arr = [js_index_by_int arr i | i <- [0 .. js_length arr - 1]]

{-# INLINEABLE emptyJSObject #-}
emptyJSObject :: JSVal
emptyJSObject = js_object_empty

{-# INLINEABLE indexJSObject #-}
indexJSObject :: JSVal -> String -> JSVal
indexJSObject obj k = js_index_by_jsref obj (toJSString k)

{-# INLINEABLE callJSObjectMethod #-}
callJSObjectMethod :: JSVal -> String -> [JSVal] -> JSVal
callJSObjectMethod obj f args =
  js_function_apply (js_index_by_jsref obj (toJSString f)) obj (toJSArray args)

{-# INLINEABLE json #-}
json :: JSVal
json = js_json

{-# INLINEABLE callJSFunction #-}
callJSFunction :: JSVal -> [JSVal] -> JSVal
callJSFunction f args = js_function_apply f js_object_empty (toJSArray args)

foreign import javascript "\"\"" js_string_empty :: JSVal

foreign import javascript "${1}.concat(${2})"
  js_concat :: JSVal -> JSVal -> JSVal

foreign import javascript "${1}.length" js_length :: JSVal -> Int

foreign import javascript "String.fromCodePoint(${1})"
  js_string_fromchar :: Char -> JSVal

foreign import javascript "${1}.codePointAt(${2})"
  js_string_tochar :: JSVal -> Int -> Char

foreign import javascript "[]" js_array_empty :: JSVal

foreign import javascript "${1}[${2}]" js_index_by_int :: JSVal -> Int -> JSVal

foreign import javascript "{}" js_object_empty :: JSVal

foreign import javascript "${1}[${2}]"
  js_index_by_jsref :: JSVal -> JSVal -> JSVal

foreign import javascript "JSON" js_json :: JSVal

foreign import javascript "${1}.apply(${2}, ${3})"
  js_function_apply :: JSVal -> JSVal -> JSVal -> JSVal
