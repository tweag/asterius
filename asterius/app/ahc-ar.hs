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
    expand opt = case opt of
      ('@' : path) -> catchIOError (lines <$> readFile path) (const $ pure [opt])
      _ -> return [opt]

-- | Create a library archive from a bunch of object files, using @Ar@ from the
-- GHC API. Though the name of each object file is preserved, we set the
-- timestamp, owner ID, group ID, and file mode to default values (0, 0, 0, and
-- 0644, respectively). When we deserialize (see @Asterius.Ar.loadAr@), the
-- metadata is ignored anyway.
createArchive :: FilePath -> [FilePath] -> IO ()
createArchive arFile objFiles = do
  blobs <- for objFiles (unsafeDupableInterleaveIO . BS.readFile)
  GHC.writeGNUAr arFile $
    GHC.Archive
      [ GHC.ArchiveEntry
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
