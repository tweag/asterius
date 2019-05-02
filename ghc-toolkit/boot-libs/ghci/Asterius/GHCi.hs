{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Asterius.GHCi
  ( asteriusRunQExp
  , asteriusRunQPat
  , asteriusRunQType
  , asteriusRunQDec
  ) where

import Asterius.ByteString
import Asterius.Types
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import GHCi.Message
import GHCi.TH.Binary ()
import qualified Language.Haskell.TH.Syntax as TH
import Prelude

newtype AsteriusQ a = AsteriusQ
  { unAsteriusQ :: ReaderT (IORef QState) IO a
  } deriving (Functor, Applicative, Monad, MonadFail, MonadIO)

instance TH.Quasi AsteriusQ

emptyQState :: QState
emptyQState = QState mempty Nothing (error "Asterius.GHCi: no pipe")

asteriusRunQ :: Binary a => TH.Q a -> IO JSArrayBuffer
asteriusRunQ m = do
  qs_ref <- newIORef emptyQState
  r <- runReaderT (unAsteriusQ (TH.runQ m)) qs_ref
  pure (byteStringToJSArrayBuffer (LBS.toStrict (encode r)))

asteriusRunQExp :: TH.Q TH.Exp -> IO JSArrayBuffer
asteriusRunQExp = asteriusRunQ

asteriusRunQPat :: TH.Q TH.Pat -> IO JSArrayBuffer
asteriusRunQPat = asteriusRunQ

asteriusRunQType :: TH.Q TH.Type -> IO JSArrayBuffer
asteriusRunQType = asteriusRunQ

asteriusRunQDec :: TH.Q [TH.Dec] -> IO JSArrayBuffer
asteriusRunQDec = asteriusRunQ
