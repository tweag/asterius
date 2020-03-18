{-# LANGUAGE LambdaCase #-}

import Asterius.Types
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Coerce
import System.Mem

foreign import javascript "console.log($1)" js_print :: JSVal -> IO ()

printString :: String -> IO ()
printString s = js_print (coerce (toJSString s))

main :: IO ()
main = do
  let ps s = do
        tid <- myThreadId
        printString (show tid <> ": " <> s)
  mvar <- newEmptyMVar
  ps "Creating workers"
  forkIO $ do
    ps "Working..."
    threadDelay 4000000
    ps "Writing result"
    tid <- myThreadId
    putMVar mvar tid
  forkIO $ do
    ps "Working..."
    threadDelay 3000000
    ps "Writing result"
    tid <- myThreadId
    putMVar mvar tid
  ps "Waiting for results"
  v1 <- takeMVar mvar
  ps ("First result: " <> show v1)
  v2 <- takeMVar mvar
  ps ("Second result: " <> show v2)
  forkIO $ do
    v3 <- takeMVar mvar
    ps ("WAT? " ++ show v3)
  performGC
