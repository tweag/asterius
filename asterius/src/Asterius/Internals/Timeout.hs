module Asterius.Internals.Timeout where

import Control.Concurrent
import Control.Exception
import Control.Monad

timeout :: Int -> IO a -> IO a
timeout t m = do
  box <- newEmptyMVar
  tid0 <- forkIO $ do
    r <- try m
    putMVar box $ case r of
      Left (SomeException err) -> throw err
      Right x -> x
  _ <- forkIO $ do
    threadDelay (t * 1000000)
    f <- tryPutMVar box (error "TIMEOUT")
    when f $ killThread tid0
  evaluate =<< readMVar box
