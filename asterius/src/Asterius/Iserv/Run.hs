{-# LANGUAGE GADTs #-}

module Asterius.Iserv.Run
  ( run
  ) where

import Asterius.Iserv.State
import Data.ByteString (ByteString)
import Data.IORef
import GHCi.Message
import GHCi.RemoteTypes
import qualified Language.Haskell.TH.Syntax as TH
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

run :: IservState -> Pipe -> Message a -> IO a
run s pipe msg =
  case msg of
    InitLinker -> pure ()
    LoadDLL _ -> pure Nothing
    LoadArchive p -> addArchive s p
    LoadObj p -> addObj s p
    AddLibrarySearchPath _ -> pure $ RemotePtr 0
    RemoveLibrarySearchPath _ -> pure True
    ResolveObjs -> pure True
    FindSystemLibrary _ -> pure $ Just ""
    CreateBCOs [sym_buf, m_buf] -> (: []) <$> createSplice s sym_buf m_buf
    StartTH -> startTH
    RunTH rstate rhv ty mb_loc -> runTH pipe rstate rhv ty mb_loc
    _ -> fail $ "Asterius.Iserv.Run.run: unsupported message: " <> show msg
