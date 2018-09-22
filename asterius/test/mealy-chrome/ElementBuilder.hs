{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wall -ddump-to-file -ddump-rn -ddump-foreign -ddump-stg -ddump-cmm-raw -ddump-asm #-}

module ElementBuilder
  ( Element(..)
  , emptyElement
  , buildElement
  ) where

import Control.Monad
import Data.Foldable
import WebAPI

import AsteriusPrim

data Element
  = Element { className :: String
            , attributes :: [(String, String)]
            , children :: [Element]
            , hidden :: Bool
            , eventHandlers :: [(String, JSRef -> IO ())] }
  | TextNode String

emptyElement :: Element
emptyElement =
  Element
    { className = ""
    , attributes = mempty
    , children = mempty
    , hidden = False
    , eventHandlers = mempty
    }

{-# INLINEABLE buildElement #-}
buildElement :: Element -> IO JSRef
buildElement Element {..} = do
  e <- createElement className
  for_ attributes $ uncurry $ setAttribute e
  for_ children $ buildElement >=> appendChild e
  when hidden $ setHidden e True
  for_ eventHandlers $ uncurry $ addEventListener e
  pure e
buildElement (TextNode s) = createTextNode s
