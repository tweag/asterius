module Cloudflare.Response
  ( JSResponse (..),
    makeResponse,
  )
where

import Asterius.Aeson
import Asterius.ByteString
import Asterius.Types
import qualified Data.ByteString.Char8 as CBS
import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Types as H
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Cache as Wai

newtype JSResponse = JSResponse JSVal

makeResponse :: Wai.Response -> IO JSResponse
makeResponse resp = do
  resp_buf <- lazyByteStringToJSUint8Array <$> Wai.responseToLBS resp
  let resp_status = Wai.responseStatus resp
      resp_status_code = H.statusCode resp_status
      resp_status_text = toJSString $ CBS.unpack $ H.statusMessage resp_status
      resp_headers =
        jsonToJSVal $
          [ (CBS.unpack $ CI.original k, CBS.unpack v)
            | (k, v) <- Wai.responseHeaders resp
          ]
  js_resp_new resp_buf resp_status_code resp_status_text resp_headers

foreign import javascript unsafe "new Response($1, {status: $2, statusText: $3, headers: new Headers($4)})" js_resp_new :: JSUint8Array -> Int -> JSString -> JSVal -> IO JSResponse
