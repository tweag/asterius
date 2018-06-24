{-# OPTIONS_GHC -Wall -O2 -ddump-to-file -ddump-stg -ddump-cmm-raw -ddump-asm #-}

import Data.Functor

foreign import javascript "get_something_js()" get_something_js :: JSRef

foreign import javascript "${1}(${2})" js_app :: JSRef -> JSRef -> JSRef

getSomethingJS :: IO JSRef
getSomethingJS = get_something_js

jsApp :: JSRef -> JSRef -> IO JSRef
jsApp = js_app

main :: IO ()
main = do
  f <- getSomethingJS
  x <- getSomethingJS
  void $ jsApp f x

