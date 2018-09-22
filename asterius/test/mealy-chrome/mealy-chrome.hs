{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wall -ddump-to-file -ddump-rn -ddump-foreign -ddump-stg -ddump-cmm-raw -ddump-asm #-}

module Main
  ( main
  , initModel
  , mealy
  ) where

import Data.List

type JSString = JSRef

type JSObject = JSRef

type JSFunction = JSRef

data Todo = Todo
  { key, text :: JSString
  , completed :: Bool
  }

newtype TodoModel = TodoModel
  { todos :: [Todo]
  }

initModel :: IO TodoModel
initModel = pure TodoModel {todos = []}

mealy :: TodoModel -> JSObject -> (TodoModel, JSFunction)
mealy prev_model event = (next_model, instr)
  where
    next_model = prev_model
    !instr = js_sth

main :: IO ()
main = pure ()

toJSString :: String -> JSString
toJSString = foldl' (\s c -> js_concat s (js_string_fromchar c)) js_string_empty

indexJSObject :: JSObject -> String -> JSRef
indexJSObject obj k = js_index_by_jsref obj (toJSString k)

foreign import javascript "${1}.concat(${2})" js_concat
  :: JSRef -> JSRef -> JSRef

foreign import javascript "''" js_string_empty :: JSRef

foreign import javascript "String.fromCodePoint(${1})" js_string_fromchar
  :: Char -> JSRef

foreign import javascript "${1}[${2}]" js_index_by_jsref
  :: JSRef -> JSRef -> JSRef

foreign import javascript "${1} === ${2}" js_eq :: JSRef -> JSRef -> Bool

foreign import javascript "()=>{console.log(233)}" js_sth :: JSRef
