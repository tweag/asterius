-- |
-- Module      :  Main
-- Copyright   :  (c) 2018 EURL Tweag
-- License     :  All rights reserved (see LICENCE file in the distribution).
--
-- This module contains the implementation of @ahc-ar@, a system-agnostic,
-- partial implementation of GNU @ar@ for creating archive files from a set of
-- object files, using @Ar@ from the GHC API.
module Main
  ( main,
  )
where

import qualified Ar as GHC
import Asterius.Ar (arIndexFileName)
import Asterius.Binary.ByteString
import Asterius.Binary.NameCache
import Asterius.Types.DependencyMap (DependencyMap)
import qualified Data.ByteString as BS
import Data.List
import Data.Traversable
import GHC.IO.Unsafe
import System.Environment.Blank
import System.Exit
import System.FilePath
import System.IO.Error

main :: IO ()
main = do
  args <- getAhcArArgs
  case find (".a" `isSuffixOf`) args of
    Just ar -> createArchive ar $ filter (".o" `isSuffixOf`) args
    Nothing -> die "ahc-ar: no .a file passed. Exiting..."

-- | Get all command-line arguments. Arguments of the form @\@file@ are
-- replaced by the newline-separated options contained in @file@. If reading
-- @file@ fails for any reason, the original @\@file@ argument remains intact.
getAhcArArgs :: IO [String]
getAhcArArgs = getArgs >>= fmap concat . mapM expand
  where
    expand :: String -> IO [String]
    expand opt = case opt of
      ('@' : path) ->
        catchIOError
          (map undoEscapeResponseFileArg . lines <$> readFile path)
          (const $ pure [opt])
      _ -> return [opt]

-- | Un-escape a single the contents of @.rsp@ file, essentially reversing the
-- effects of Cabal's @escapeResponseFileArg@
-- (https://github.com/haskell/cabal/blob/dde6255a4e6b531c0567bc499840d11ead9d885b/Cabal/Distribution/Simple/Program/ResponseFile.hs#L48).
undoEscapeResponseFileArg :: String -> String
undoEscapeResponseFileArg arg = case arg of
  "" -> ""
  '\\' : c : cs -> c : undoEscapeResponseFileArg cs
  '\\' : [] -> error "undoEscapeResponseFileArg: dangling backslash"
  c : cs -> c : undoEscapeResponseFileArg cs

createIndex :: [BS.ByteString] -> IO DependencyMap
createIndex blobs = do
  ncu <- newNameCacheUpdater
  mconcat <$> for blobs (getBS ncu) -- Get the dependencyMap only.

createIndexEntry :: [BS.ByteString] -> IO GHC.ArchiveEntry
createIndexEntry blobs = do
  index <- createIndex blobs
  blob <- putBS index
  pure
    GHC.ArchiveEntry
      { GHC.filename = arIndexFileName,
        GHC.filetime = 0,
        GHC.fileown = 0,
        GHC.filegrp = 0,
        GHC.filemode = 0o644,
        GHC.filesize = BS.length blob,
        GHC.filedata = blob
      }

-- | Create a library archive from a bunch of object files, using @Ar@ from the
-- GHC API. Though the name of each object file is preserved, we set the
-- timestamp, owner ID, group ID, and file mode to default values (0, 0, 0, and
-- 0644, respectively). When we deserialize (see @Asterius.Ar.loadAr@), the
-- metadata is ignored anyway.
createArchive :: FilePath -> [FilePath] -> IO ()
createArchive arFile objFiles = do
  blobs <- for objFiles (unsafeDupableInterleaveIO . BS.readFile)
  index <- createIndexEntry blobs
  GHC.writeGNUAr arFile
    $ GHC.Archive
    $ index
      : [ GHC.ArchiveEntry
            { GHC.filename = takeFileName obj_path,
              GHC.filetime = 0,
              GHC.fileown = 0,
              GHC.filegrp = 0,
              GHC.filemode = 0o644,
              GHC.filesize = BS.length blob,
              GHC.filedata = blob
            }
          | (obj_path, blob) <- zip objFiles blobs
        ]
