module Cloudflare where

import Asterius.Types
import Control.Monad
import Data.Coerce

foreign export javascript "handleFetch" handleFetch :: JSObject -> IO JSObject

handleFetch :: JSObject -> IO JSObject
handleFetch ev = do
  req <- indexJSObject ev "request"
  method <- indexJSObject (coerce req) "method"
  case (fromJSString . coerce) method of
    "POST" -> do
      let payload = js_request_json req
      name <- indexJSObject payload "name"
      if js_is_undefined name then
        pure $ js_new_response (toJSString "Name not specified") 400
      else
        pure $ js_new_response (toJSString $ "Hello " <> (fromJSString . coerce) name) 200
    _ ->
      pure $ js_new_response (toJSString "Hello from Haskell") 200

foreign import javascript "${1} === undefined"
  js_is_undefined :: JSVal -> Bool

foreign import javascript safe "${1}.json()"
  js_request_json :: JSVal -> JSObject

foreign import javascript "new Response(${1}, {\"status\": ${2}})"
  js_new_response :: JSString -> Int -> JSObject
