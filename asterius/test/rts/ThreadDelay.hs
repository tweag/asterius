import Asterius.Types
import Data.Coerce
import Control.Concurrent

foreign import javascript "console.log(${1})" js_print :: JSVal -> IO ()

printString :: String -> IO ()
printString s = js_print (coerce (toJSString s))

main :: IO ()
main = do
   printString "Hello"
   threadDelay 2000000
   printString "world"
   threadDelay 2000000
   printString "of delays!"

