{-# LANGUAGE OverloadedStrings #-}

module NoMain where

import Asterius.ByteString
import Asterius.Types
import System.IO.Unsafe

{-# NOINLINE x #-}
x :: JSUint8Array
x = unsafePerformIO $ do
  love
  pure $ byteStringToJSUint8Array "Lorem ipsum"

foreign import javascript "console.error('From node, with love')" love :: IO ()
