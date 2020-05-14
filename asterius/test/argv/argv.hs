import Asterius.Types
import Data.Coerce
import System.Environment

foreign import javascript "console.log($1)" js_print :: JSVal -> IO ()

js_print_string :: String -> IO ()
js_print_string s = js_print (coerce (toJSString s))

main :: IO ()
main = getArgs >>= print  -- expected output: ["extra","flags"]
