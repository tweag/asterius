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
-- all fields, except the object file identifier and the file size, we use
-- default values, as follows:
--
-- > ---------+-----------------------------+----------------+
-- >    SIZE  |         DESCRIPTION         |    CONTENTS    |
-- > ---------+-----------------------------+----------------+
-- > 16 bytes | File identifier             | FILENAME       |
-- > 12 bytes | File modification timestamp | "0           " |
-- >  6 bytes | Owner ID                    | "0     "       |
-- >  6 bytes | Group ID                    | "0     "       |
-- >  8 bytes | File mode                   | "0644    "     |
-- > 10 bytes | File size                   | FILESIZE       |
-- >  2 bytes | "Magic" ending characters   | "\x60\x0a"     |
-- > ---------+-----------------------------+----------------+
-- > 60 bytes | TOTAL                       |
-- > ---------+-----------------------------+
module Main
  ( main,
  )
where

import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (find, isSuffixOf)
import System.Directory (doesFileExist)
import System.Environment.Blank
import System.Exit (die)
import System.IO (Handle, IOMode (..), hFileSize, hFlush, withFile)
import System.IO.Error (catchIOError)
import System.Process (callProcess)

-- TODOs:
--   Add proper checks (doesFileExist, etc.)
--   Add proper flag parsing
--   Maybe work on a temporary file first and copy the results afterwards?
--   getArgsRecursively: adapt to be able to parse quoted arguments as specified by @man ar@?

-- Some resources:
--   https://github.com/haskell/cabal/blob/master/Cabal/Distribution/Simple/Program/Ar.hs
--   https://en.wikipedia.org/wiki/Ar_(Unix)
--   https://pubs.opengroup.org/onlinepubs/007908799/xcu/ar.html
--   https://www.freebsd.org/cgi/man.cgi?query=ar&sektion=5&manpath=4.3BSD+NET%2F2

-- NOTE TAKEN FROM Distribution.Simple.Program.Ar:
--
--   The args to use with "ar" are actually rather subtle and system-dependent.
--   In particular we have the following issues:
--
--    -- On OS X, "ar q" does not make an archive index. Archives with no
--       index cannot be used.
--
--    -- GNU "ar r" will not let us add duplicate objects, only "ar q" lets us
--       do that. We have duplicates because of modules like "A.M" and "B.M"
--       both make an object file "M.o" and ar does not consider the directory.
--
--   Our solution is to use "ar r" in the simple case when one call is enough.
--   When we need to call ar multiple times we use "ar q" and for the last
--   call on OSX we use "ar qs" so that it'll make the index.

main :: IO ()
main = ahcAr

-- gnuAr :: IO ()
-- gnuAr = getArgs >>= callProcess "ar"

ahcAr :: IO ()
ahcAr = do
  args <- getArgsRecursively
  case find (".a" `isSuffixOf`) args of
    Just ar -> createArchive ar $ filter (".o" `isSuffixOf`) args
    Nothing -> die "ahc-ar: no .a file passed. Exiting..."

-- | Get all arguments, recursively. This function should look through
-- arguments prefixed with \@, as described in @man ar@:
--
-- Read command-line options from file. The options read are inserted in place of
-- the original @file option. If file does not exist, or cannot be read, then the
-- option will be treated literally, and not removed.
--
-- Options in file are separated by whitespace. A whitespace character may be
-- included in an option by surrounding the entire option in either single or
-- double quotes. Any character (including a backslash) may be included by
-- prefixing the character to be included with a backslash. The file may itself
-- contain additional @file options; any such options will be processed
-- recursively.
getArgsRecursively :: IO [String]
getArgsRecursively = getArgs >>= concatMapM expandAtOption
  where
    expandAtOption :: String -> IO [String]
    expandAtOption opt = case opt of
      ('@' : path) -> doesFileExist path >>= \case
        True ->
          catchIOError
            (readFile path >>= concatMapM expandAtOption . words)
            (\_ -> return [opt])
        False -> return [opt]
      _ -> return [opt]

-- TODO: doesn't this thing exist already somewhere?
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f
{-# INLINE concatMapM #-}

-----------------------------------------------------------------------------

-- | Create a library archive from a bunch of object files.
createArchive :: FilePath -> [FilePath] -> IO ()
createArchive arFile objFiles =
  withFile arFile WriteMode $ \ah -> do
    BS.hPut ah "!<arch>\x0a"
    forM_ objFiles $ hPutObjFile ah
  where
    hPutObjFile :: Handle -> FilePath -> IO ()
    hPutObjFile ah filename = do
      withFile filename ReadMode $ \oh -> do
        fileSize <- hFileSize oh
        -- Set the file metadata
        BS.hPut ah $ BSC.pack $ take 16 $ filename ++ repeat ' '
        BS.hPut ah "0           "
        BS.hPut ah "0     "
        BS.hPut ah "0     "
        BS.hPut ah "0644    "
        BS.hPut ah $ BSC.pack $ take 10 $ show fileSize ++ repeat ' '
        BS.hPut ah "\x60\x0a"
        -- Copy the contents
        BS.hGetContents oh >>= BS.hPut ah
        when (odd fileSize) $ BS.hPut ah "\x0a"
        hFlush ah
