{-# LANGUAGE StrictData #-}

module Worker (handleFetch) where

import Asterius.Types

foreign export javascript "handleFetch" handleFetch :: JSObject -> IO JSObject

handleFetch :: JSObject -> IO JSObject
handleFetch ev = do
  undefined
