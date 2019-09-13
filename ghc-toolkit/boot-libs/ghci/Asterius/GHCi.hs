module Asterius.GHCi
  ( asteriusRunQExp,
    asteriusRunQPat,
    asteriusRunQType,
    asteriusRunQDec
    )
where

import Asterius.ByteString
import Asterius.Types
import Control.DeepSeq
import Control.Exception
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import GHC.IO.Device
import GHC.IO.Handle.FD
import GHCi.Message
import GHCi.RemoteTypes
import GHCi.TH
import Language.Haskell.TH.Syntax
import System.IO
import System.IO.Unsafe

asteriusRunQ :: THResultType -> Q a -> IO JSArrayBuffer
asteriusRunQ ty hv = do
  r <-
    try $ do
      rstate <- startTH
      rhv <- toHValueRef <$> mkRemoteRef hv
      runTH globalPipe rstate rhv ty Nothing
  byteStringToJSArrayBuffer . LBS.toStrict . encode <$> case r of
    Left e
      | Just (GHCiQException _ err) <- fromException e -> pure $ QFail err
      | otherwise -> QException <$> showException e
    Right a -> pure $ QDone a

showException :: SomeException -> IO String
showException e0 = do
  r <- try $ evaluate $ force $ show e0
  case r of
    Left e -> showException e
    Right str -> pure str

asteriusRunQExp :: Q Exp -> IO JSArrayBuffer
asteriusRunQExp = asteriusRunQ THExp

asteriusRunQPat :: Q Pat -> IO JSArrayBuffer
asteriusRunQPat = asteriusRunQ THPat

asteriusRunQType :: Q Type -> IO JSArrayBuffer
asteriusRunQType = asteriusRunQ THType

asteriusRunQDec :: Q [Dec] -> IO JSArrayBuffer
asteriusRunQDec = asteriusRunQ THDec

{-# NOINLINE globalPipe #-}
globalPipe :: Pipe
globalPipe = unsafePerformIO $ do
  read_handle <-
    fdToHandle' (fromIntegral read_fd)
      (Just Stream)
      False
      ""
      ReadMode
      True
  write_handle <-
    fdToHandle' (fromIntegral write_fd)
      (Just Stream)
      False
      ""
      WriteMode
      True
  hSetBuffering read_handle NoBuffering
  hSetBuffering write_handle NoBuffering
  lo_ref <- newIORef Nothing
  pure Pipe
    { pipeRead = read_handle,
      pipeWrite = write_handle,
      pipeLeftovers = lo_ref
      }

foreign import javascript "Number(process.env.ASTERIUS_NODE_READ_FD)" read_fd :: Int

foreign import javascript "Number(process.env.ASTERIUS_NODE_WRITE_FD)" write_fd :: Int
