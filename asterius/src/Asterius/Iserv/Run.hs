{-# LANGUAGE GADTs #-}

module Asterius.Iserv.Run
  ( run
  ) where

import Control.Monad.Fail
import Data.ByteString (ByteString)
import Data.IORef
import GHCi.Message
import GHCi.RemoteTypes
import qualified Language.Haskell.TH.Syntax as TH
import Prelude hiding (fail)
import Unsafe.Coerce

startTH :: IO (RemoteRef (IORef QState))
startTH = pure $ unsafeCoerce $ RemotePtr 0

runTH ::
     Pipe
  -> RemoteRef (IORef QState)
  -> HValueRef
  -> THResultType
  -> Maybe TH.Loc
  -> IO (QResult ByteString)
runTH pipe _ _ _ _ = do
  writePipe pipe $ putTHMessage RunTHDone
  pure $ QFail "Asterius.Iserv.Run.runTH"

run :: Pipe -> Message a -> IO a
run pipe msg =
  case msg of
    InitLinker -> pure ()
    LoadArchive _ -> pure ()
    AddLibrarySearchPath _ -> pure $ RemotePtr 0
    RemoveLibrarySearchPath _ -> pure True
    ResolveObjs -> pure True
    StartTH -> startTH
    RunTH rstate rhv ty mb_loc -> runTH pipe rstate rhv ty mb_loc
    _ -> fail $ "Asterius.Iserv.Run.run: unsupported message: " <> show msg
