module Cloudflare where

import Asterius.Types
import Control.Monad
import Data.Coerce

foreign export javascript "handleFetch" handleFetch :: JSObject -> IO JSObject

handleFetch :: JSObject -> IO JSObject
handleFetch ev = do
  req <- indexJSObject ev "request"
  let req_str = jsonStringify req
  pure $ js_new_response $ toJSString $ "From Haskell: " <> req_str


foreign import javascript "new Response(${1})"
  js_new_response :: JSString -> JSObject

