{-# LANGUAGE NoImplicitPrelude #-}

module Asterius.Types.JSFunction
  ( JSFunction (..),
    freeHaskellCallback,
  )
where

import Asterius.Types.JSString ()
import Asterius.Types.JSVal
import GHC.Base
import GHC.Show

newtype JSFunction
  = JSFunction JSVal
  deriving (Show)

foreign import ccall unsafe "freeHaskellCallback"
  freeHaskellCallback :: JSFunction -> IO ()
