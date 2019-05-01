{-# LANGUAGE OverloadedStrings #-}

module NoMain where

import Asterius.ByteString
import Asterius.Types

x :: JSArrayBuffer
x = byteStringToJSArrayBuffer "Lorem ipsum"
