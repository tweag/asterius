{-# OPTIONS_GHC -Wno-deprecations #-}

module Cloudflare.Request
  ( JSRequest (..),
    parseRequest,
  )
where

import Asterius.Aeson
import Asterius.ByteString
import Asterius.Text
import Asterius.Types
import Control.Exception
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import Data.Coerce
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as H
import qualified Network.HTTP.Types.Header as H
import qualified Network.Wai as Wai

newtype JSEvent
  = JSEvent JSVal

newtype JSRequest
  = JSRequest JSVal

parseRequest :: JSRequest -> IO Wai.Request
parseRequest req = do
  let _url = T.encodeUtf8 $ req_url req
      _raw_path = H.extractPath _url
      (_path_segs, _query) = H.decodePath _raw_path
      _headers =
        [(CI.mk $ T.encodeUtf8 k, T.encodeUtf8 v) | (k, v) <- req_headers req]
  evaluate $
    Wai.defaultRequest
      { Wai.requestMethod = T.encodeUtf8 $ req_method req,
        Wai.rawPathInfo = _raw_path,
        Wai.rawQueryString = H.renderQuery True _query,
        Wai.requestHeaders = _headers,
        Wai.pathInfo = _path_segs,
        Wai.queryString = _query,
        Wai.requestBody = req_body req,
        Wai.requestBodyLength = Wai.ChunkedBody,
        Wai.requestHeaderHost = lookup H.hHost _headers,
        Wai.requestHeaderRange = lookup H.hRange _headers,
        Wai.requestHeaderReferer = lookup H.hReferer _headers,
        Wai.requestHeaderUserAgent = lookup H.hUserAgent _headers
      }

req_method :: JSRequest -> T.Text
req_method = textFromJSString . js_req_method

req_url :: JSRequest -> T.Text
req_url = textFromJSString . js_req_url

req_headers :: JSRequest -> [(T.Text, T.Text)]
req_headers = jsonFromJSVal' . js_req_headers

req_body :: JSRequest -> IO BS.ByteString
req_body js_req = byteStringFromJSUint8Array <$> js_req_body js_req

foreign import javascript unsafe "$1.method"
  js_req_method ::
    JSRequest -> JSString

foreign import javascript unsafe "$1.url" js_req_url :: JSRequest -> JSString

foreign import javascript unsafe "Array.from($1.headers.entries())"
  js_req_headers ::
    JSRequest -> JSVal

foreign import javascript safe "new Uint8Array($1.arrayBuffer())"
  js_req_body ::
    JSRequest -> IO JSUint8Array
