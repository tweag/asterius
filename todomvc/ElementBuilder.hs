{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module ElementBuilder
  ( Element (..),
    emptyElement,
    buildElement,
  )
where

import Asterius.Types
import Control.Monad
import Data.Foldable
import WebAPI

data Element
  = Element
      { className :: String,
        attributes :: [(String, String)],
        children :: [Element],
        hidden :: Bool,
        eventHandlers :: [(String, JSObject -> IO ())]
      }
  | TextNode String

emptyElement :: Element
emptyElement = Element
  { className = "",
    attributes = mempty,
    children = mempty,
    hidden = False,
    eventHandlers = mempty
  }

buildElement :: Element -> IO JSVal
buildElement Element {..} = do
  e <- createElement className
  for_ attributes $ uncurry $ setAttribute e
  for_ children $ buildElement >=> appendChild e
  when hidden $ setHidden e True
  for_ eventHandlers $ uncurry $ addEventListener e
  pure e
buildElement (TextNode s) = createTextNode s
