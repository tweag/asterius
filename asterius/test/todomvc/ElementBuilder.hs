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
import qualified Data.Map.Strict as Map
import WebAPI

import AsteriusPrim

data Element
  = Element { className :: String
            , attributes :: Map.Map String String
            , children :: [Element]
            , hidden :: Bool
            , eventHandlers :: Map.Map String (JSRef -> IO ()) }
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
  for_ (Map.toList attributes) $ uncurry $ setAttribute e
  for_ children $ buildElement >=> appendChild e
  when hidden $ setHidden e True
  for_ (Map.toList eventHandlers) $ uncurry $ addEventListener e
  pure e
buildElement (TextNode s) = createTextNode s
