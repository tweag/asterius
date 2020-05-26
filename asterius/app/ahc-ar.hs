{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :  Main
-- Copyright   :  (c) 2018 EURL Tweag
-- License     :  All rights reserved (see LICENCE file in the distribution).
--
-- This module contains the implementation of @ahc-ar@, a system-agnostic,
-- partial implementation of GNU @ar@ for creating archive files from a set of
-- object files. The shape of the contents of the generated archive files is as
-- explained [here](https://en.wikipedia.org/wiki/Ar_(Unix)#File_header). For
-- all fields (except the file size), we use default values, as follows:
--
-- > ---------+-----------------------------+----------------+
-- >    SIZE  |         DESCRIPTION         |    CONTENTS    |
-- > ---------+-----------------------------+----------------+
-- > 16 bytes | File identifier             | "whatever"     |
-- > 12 bytes | File modification timestamp | 0              |
-- >  6 bytes | Owner ID                    | 0              |
-- >  6 bytes | Group ID                    | 0              |
-- >  8 bytes | File mode                   | 0644           |
-- > 10 bytes | File size                   | FILESIZE       |
-- >  2 bytes | "Magic" ending characters   | "\x60\x0a"     |
-- > ---------+-----------------------------+----------------+
-- > 60 bytes | TOTAL                       |
-- > ---------+-----------------------------+
module Main
  ( main,
  )
where

import qualified Ar as GHC
import Asterius.Binary.ByteString
import Asterius.Binary.File
import Asterius.Binary.NameCache
import Asterius.Types
import qualified Data.ByteString as BS
import Data.Either
import Data.List
import Data.Traversable
import System.Environment.Blank
import System.Exit
import System.IO.Error

main :: IO ()
main = do
  args <- getAhcArArgs
  case find (".a" `isSuffixOf`) args of
    Just ar -> createArchive ar $ filter (".o" `isSuffixOf`) args
    Nothing -> die "ahc-ar: no .a file passed. Exiting..."

-- | Get all arguments. Command-line arguments are also read from a file
-- (prefixed by \@) non-recursively. Options in a file are line-separated and
-- cannot contain \@-arguments themselves.
getAhcArArgs :: IO [String]
getAhcArArgs = getArgs >>= fmap concat . mapM expand
  where
    expand opt = case opt of
      ('@' : path) -> catchIOError (lines <$> readFile path) (const $ pure [opt])
      _ -> return [opt]

-- | Create a library archive from a bunch of object files. Instead of
-- following the traditional approach of making the archive contain the input
-- objects, we merge all object files into one before creating the archive.
-- Effectively, this means that (a) the archive files we create always contain
-- a single object file, and (b) it is not possible to retrieve the individual
-- original object files from the archive, only their combination.
createArchive :: FilePath -> [FilePath] -> IO ()
createArchive arFile objFiles = do
  ncu <- newNameCacheUpdater
  objs <- rights <$> for objFiles (tryGetFile ncu)
  contents <- putBS (mconcat objs :: AsteriusCachedModule)
  GHC.writeGNUAr arFile $
    GHC.Archive
      [ GHC.ArchiveEntry
          { GHC.filename = "whatever",
            GHC.filetime = 0,
            GHC.fileown = 0,
            GHC.filegrp = 0,
            GHC.filemode = 0o644,
            GHC.filesize = BS.length contents,
            GHC.filedata = contents
          }
      ]
