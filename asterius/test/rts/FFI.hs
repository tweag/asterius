{-# LANGUAGE ScopedTypeVariables #-}

import Asterius.Types
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Coerce

foreign import javascript "console.log($1)" js_print :: JSVal -> IO ()

foreign import javascript safe "(() => {throw 'FAILED'})()" js_failed :: IO ()

printString :: String -> IO ()
printString s = js_print (coerce (toJSString s))

main :: IO ()
main = do
  js_failed `catch` \(e :: JSException) -> do
    printString ("Exception caught in safe FFI: " ++ show e)
