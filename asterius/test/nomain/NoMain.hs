{-# LANGUAGE OverloadedStrings #-}

module NoMain where

import Asterius.ByteString
import Asterius.Types
import System.IO.Unsafe

{-# NOINLINE x #-}
x :: JSArrayBuffer
x = unsafePerformIO $ do
  love
  pure $ byteStringToJSArrayBuffer "Lorem ipsum"

foreign import javascript "console.error('From node, with love')" love :: IO ()
