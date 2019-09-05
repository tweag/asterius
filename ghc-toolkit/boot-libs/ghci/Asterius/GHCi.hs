module Asterius.GHCi
  ( asteriusRunQExp,
    asteriusRunQPat,
    asteriusRunQType,
    asteriusRunQDec
    )
where

import Asterius.ByteString
import Asterius.Types
import GHCi.Message
import GHCi.RemoteTypes
import GHCi.TH
import Language.Haskell.TH.Syntax

asteriusRunQ :: THResultType -> Q a -> IO JSArrayBuffer
asteriusRunQ ty hv = do
  rstate <- startTH
  rhv <- toHValueRef <$> mkRemoteRef hv
  byteStringToJSArrayBuffer
    <$> runTH (error "asteriusRunQ: no pipe") rstate rhv ty Nothing

asteriusRunQExp :: Q Exp -> IO JSArrayBuffer
asteriusRunQExp = asteriusRunQ THExp

asteriusRunQPat :: Q Pat -> IO JSArrayBuffer
asteriusRunQPat = asteriusRunQ THPat

asteriusRunQType :: Q Type -> IO JSArrayBuffer
asteriusRunQType = asteriusRunQ THType

asteriusRunQDec :: Q [Dec] -> IO JSArrayBuffer
asteriusRunQDec = asteriusRunQ THDec
