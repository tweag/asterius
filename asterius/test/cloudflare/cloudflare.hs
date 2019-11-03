import Asterius.Types
import Control.Monad
import Data.Coerce

handleFetch :: (JSObject -> IO ()) -> IO ()
handleFetch = coerce makeHaskellCallback1 >=> js_handle_fetch

main :: IO ()
main = handleFetch $ \ev -> do
  req <- indexJSObject ev "request"
  let req_str = jsonStringify req
      resp = js_new_response $ toJSString $ "From Haskell: " <> req_str
      resp_promise = js_promise_resolve resp
  js_respond_with ev resp_promise

foreign import javascript "addEventListener('fetch', ${1})"
  js_handle_fetch :: JSFunction -> IO ()

foreign import javascript "new Response(${1})"
  js_new_response :: JSString -> JSObject

foreign import javascript "Promise.resolve(${1})"
  js_promise_resolve :: JSObject -> JSObject

foreign import javascript "${1}.respondWith(${2})"
  js_respond_with :: JSObject -> JSObject -> IO ()
