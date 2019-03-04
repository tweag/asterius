{-# LANGUAGE GADTs #-}

module Asterius.Iserv.Run
  ( run
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Traversable
import GHCi.Message
import GHCi.RemoteTypes
import qualified Language.Haskell.TH.Syntax as TH
import Unsafe.Coerce

initLinker :: IO ()
initLinker = pure ()

lookupSymbol :: String -> IO (Maybe (RemotePtr ()))
lookupSymbol _ = pure $ Just $ RemotePtr 0

loadArchive :: String -> IO ()
loadArchive _ = pure ()

addLibrarySearchPath :: String -> IO (RemotePtr ())
addLibrarySearchPath _ = pure $ RemotePtr 0

removeLibrarySearchPath :: RemotePtr () -> IO Bool
removeLibrarySearchPath _ = pure True

resolveObjs :: IO Bool
resolveObjs = pure True

mallocData :: ByteString -> IO (RemotePtr ())
mallocData _ = pure $ RemotePtr 0

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
    InitLinker -> initLinker
    LookupSymbol str -> lookupSymbol str
    LoadArchive str -> loadArchive str
    AddLibrarySearchPath str -> addLibrarySearchPath str
    RemoveLibrarySearchPath ptr -> removeLibrarySearchPath ptr
    ResolveObjs -> resolveObjs
    MallocData bs -> mallocData bs
    MallocStrings bss -> for bss $ mallocData . (`BS.snoc` 0)
    StartTH -> startTH
    RunTH rstate rhv ty mb_loc -> runTH pipe rstate rhv ty mb_loc
    _ -> undefined
