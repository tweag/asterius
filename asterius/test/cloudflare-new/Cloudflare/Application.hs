module Cloudflare.Application
  ( fromWaiApplication,
  )
where

import Cloudflare.Request
import Cloudflare.Response
import Control.Concurrent
import Control.Exception
import GHC.IO (catchAny)
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai

fromWaiApplication :: Wai.Application -> JSRequest -> IO JSResponse
fromWaiApplication app js_req = do
  req <- parseRequest js_req
  resp_box <- newEmptyMVar
  app req $ \resp -> do
    putMVar resp_box resp
    pure Wai.ResponseReceived
  makeResponse =<< takeMVar resp_box
