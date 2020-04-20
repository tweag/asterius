{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.GHCi
  ( asteriusRunQExp,
    asteriusRunQPat,
    asteriusRunQType,
    asteriusRunQDec,
    asteriusRunAnnWrapper,
    asteriusRunModFinalizers,
  )
where

import Control.DeepSeq
import Control.Exception
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import GHC.Desugar
import GHC.IO.Device
import GHC.IO.Handle.FD
import GHCi.Message
import GHCi.RemoteTypes
import GHCi.TH
import Language.Haskell.TH.Syntax
import System.IO
import System.IO.Unsafe
import Prelude

asteriusRunQ :: BS.ByteString -> THResultType -> a -> IO ()
asteriusRunQ loc_bs ty hv = do
  r <- try $ do
    rstate <- startTH
    rhv <- toHValueRef <$> mkRemoteRef hv
    runTH globalPipe rstate rhv ty (decode (LBS.fromStrict loc_bs))
  resp <- case r of
    Left e
      | Just (GHCiQException _ err) <- fromException e -> pure $ QFail err
      | otherwise -> QException <$> showException e
    Right (a :: BS.ByteString) -> pure $ QDone a
  writePipe globalPipe $ do
    putTHMessage RunTHDone
    put resp

showException :: SomeException -> IO String
showException e0 = do
  r <- try $ evaluate $ force $ show e0
  case r of
    Left e -> showException e
    Right str -> pure str

asteriusRunQExp :: BS.ByteString -> Q Exp -> IO ()
asteriusRunQExp loc_bs = asteriusRunQ loc_bs THExp

asteriusRunQPat :: BS.ByteString -> Q Pat -> IO ()
asteriusRunQPat loc_bs = asteriusRunQ loc_bs THPat

asteriusRunQType :: BS.ByteString -> Q Type -> IO ()
asteriusRunQType loc_bs = asteriusRunQ loc_bs THType

asteriusRunQDec :: BS.ByteString -> Q [Dec] -> IO ()
asteriusRunQDec loc_bs = asteriusRunQ loc_bs THDec

asteriusRunAnnWrapper :: BS.ByteString -> AnnotationWrapper -> IO ()
asteriusRunAnnWrapper loc_bs = asteriusRunQ loc_bs THAnnWrapper

asteriusRunModFinalizers :: IO ()
asteriusRunModFinalizers = writePipe globalPipe $ do
  putTHMessage RunTHDone
  put $ QDone ()

{-# NOINLINE globalPipe #-}
globalPipe :: Pipe
globalPipe = unsafePerformIO $ do
  read_handle <-
    fdToHandle'
      (fromIntegral read_fd)
      (Just Stream)
      False
      ""
      ReadMode
      True
  write_handle <-
    fdToHandle'
      (fromIntegral write_fd)
      (Just Stream)
      False
      ""
      WriteMode
      True
  hSetBuffering read_handle NoBuffering
  hSetBuffering write_handle NoBuffering
  lo_ref <- newIORef Nothing
  pure
    Pipe
      { pipeRead = read_handle,
        pipeWrite = write_handle,
        pipeLeftovers = lo_ref
      }

foreign import javascript "Number(process.env.ASTERIUS_NODE_READ_FD)" read_fd :: Int

foreign import javascript "Number(process.env.ASTERIUS_NODE_WRITE_FD)" write_fd :: Int
