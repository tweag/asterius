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
  ) where

import Data.List

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
indexJSArray = js_array_index

{-# INLINEABLE toJSArray #-}
toJSArray :: [JSRef] -> JSArray
toJSArray = foldl' js_concat js_array_empty

{-# INLINEABLE fromJSArray #-}
fromJSArray :: JSArray -> [JSRef]
fromJSArray arr = [js_array_index arr i | i <- [0 .. js_length arr - 1]]

foreign import javascript "\"\"" js_string_empty :: JSRef

foreign import javascript "${1}.concat(${2})" js_concat
  :: JSRef -> JSRef -> JSRef

foreign import javascript "${1}.length" js_length :: JSRef -> Int

foreign import javascript "String.fromCodePoint(${1})" js_string_fromchar
  :: Char -> JSRef

foreign import javascript "${1}.codePointAt(${2})" js_string_tochar
  :: JSRef -> Int -> Char

foreign import javascript "[]" js_array_empty :: JSRef

foreign import javascript "${1}[${2}]" js_array_index :: JSRef -> Int -> JSRef
