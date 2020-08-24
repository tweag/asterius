{-# OPTIONS_GHC -Wall #-}

module Worker
  ( handleFetch,
  )
where

import Asterius.Types
import Cloudflare.Application
import Cloudflare.Request
import Cloudflare.Response
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Network.HTTP.Types as H
import qualified Network.Wai as Wai

newtype JSEvent = JSEvent JSVal

foreign export javascript "handleFetch" handleFetch :: JSEvent -> IO JSResponse

handleFetch :: JSEvent -> IO JSResponse
handleFetch js_ev = do
  let js_req = js_event_req js_ev
  fromWaiApplication workerApp js_req

workerApp :: Wai.Application
workerApp req respond =
  respond $
    Wai.responseLBS
      H.status200
      mempty
      (LBS.pack (show (Wai.requestHeaders req)))

foreign import javascript unsafe "$1.request" js_event_req :: JSEvent -> JSRequest
