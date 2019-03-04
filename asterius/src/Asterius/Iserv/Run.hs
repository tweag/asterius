{-# LANGUAGE GADTs #-}

module Asterius.Iserv.Run
  ( run
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Traversable
import GHCi.Message
import GHCi.RemoteTypes

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

run :: Message a -> IO a
run msg =
  case msg of
    InitLinker -> initLinker
    LookupSymbol str -> lookupSymbol str
    LoadArchive str -> loadArchive str
    AddLibrarySearchPath str -> addLibrarySearchPath str
    RemoveLibrarySearchPath ptr -> removeLibrarySearchPath ptr
    ResolveObjs -> resolveObjs
    MallocData bs -> mallocData bs
    MallocStrings bss -> for bss $ mallocData . (`BS.snoc` 0)
    _ -> undefined
