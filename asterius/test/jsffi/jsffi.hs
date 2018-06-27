{-# OPTIONS_GHC -Wall -O2 -ddump-to-file -ddump-stg -ddump-cmm-raw -ddump-asm #-}

foreign import javascript "get_something_js()" get_something_js :: IO JSRef

foreign import javascript "${1}(${2})" js_app :: JSRef -> JSRef -> IO JSRef

foreign import javascript "${1}(${2})" js_app_anon :: JSRef -> JSRef -> IO ()

main :: IO ()
main = do
  f <- get_something_js
  x <- get_something_js
  y <- js_app f x
  js_app_anon f y
