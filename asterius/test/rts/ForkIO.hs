import Asterius.Types
import Control.Concurrent
import Control.Monad
import Data.Coerce

foreign import javascript "console.log($1)" js_print :: JSVal -> IO ()

printString :: String -> IO ()
printString s = js_print (coerce (toJSString s))

main :: IO ()
main = do
  let ps s = do
        tid <- myThreadId
        printString (show tid <> ": " <> s)
  ps "Hello"
  forkIO $ do
    threadDelay 2000000
    ps "world"
    threadDelay 3000000
    ps "Don't forget me!"
  threadDelay 4000000
  ps "of (forked) delays!"
  replicateM_ 10 $ forkIO $ ps "Hi!"
