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
import GHCi.Message
import GHCi.RemoteTypes
import GHCi.TH
import Language.Haskell.TH.Syntax

asteriusRunQ :: THResultType -> Q a -> IO JSArrayBuffer
asteriusRunQ ty hv = do
  r <-
    try $ do
      rstate <- startTH
      rhv <- toHValueRef <$> mkRemoteRef hv
      runTH Pipe
        { pipeRead = error "asteriusRunQ: no pipeRead",
          pipeWrite = error "asteriusRunQ: no pipeWrite",
          pipeLeftovers = error "asteriusRunQ: no pipeLeftovers"
          }
        rstate
        rhv
        ty
        Nothing
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
