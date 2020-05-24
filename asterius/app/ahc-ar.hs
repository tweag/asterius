{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (find, isSuffixOf)
import System.Directory (doesFileExist)
import System.Environment.Blank
import System.Exit (die)
import System.FilePath (splitFileName)
import System.IO (Handle, IOMode (..), hFileSize, hFlush, withFile)
import System.IO.Error (catchIOError)
import System.Process (callProcess)

-- TODOs:
--   Add proper checks (doesFileExist, etc.)
--   Add proper flag parsing
--   Maybe work on a temporary file first and copy the results afterwards?
--   Cleanup and ormolize
--   Still unclear: should we use withBinaryFile etc. instead?
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
main = ahcAr -- gnuAr

gnuAr :: IO ()
gnuAr = getArgs >>= callProcess "ar"

ahcAr :: IO ()
ahcAr = do
  args <- getArgsRecursively
  let object_files = filter (".o" `isSuffixOf`) args
  case find (".a" `isSuffixOf`) args of
    Just ar -> createArchive ar object_files
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
    BS.hPut ah "!<arch>\x0a" -- ASCII Magic String (8 bytes)
    forM_ objFiles $ hPutObjFile ah
  where
    hPutObjFile :: Handle -> FilePath -> IO ()
    hPutObjFile ah filename = do
      withFile filename ReadMode $ \oh -> do
        let fileID = BSC.pack $ take 16 $ filename ++ repeat ' '
        fileSize <- hFileSize oh
        let bsFileSize = BSC.pack $ take 10 $ show fileSize ++ repeat ' '
        hPutFileHeader ah fileID bsFileSize
        hCopyContents fileSize oh ah

    -- TODO: assert that filename and filesize have the proper size.
    hPutFileHeader :: Handle -> BS.ByteString -> BS.ByteString -> IO ()
    hPutFileHeader h filename filesize = do
      BS.hPut h filename -- File identifier (16 bytes)
      BS.hPut h "0           " -- File modification timestamp (12 bytes)
      BS.hPut h "0     " -- Owner ID (6 bytes)
      BS.hPut h "0     " -- Group ID (6 bytes)
      BS.hPut h "0644    " -- File mode (8 bytes)
      BS.hPut h filesize -- File size (10 bytes)
      BS.hPut h "\x60\x0a" -- "Magic" ending characters (2 bytes)

    -- TODO: this doesn't look great, but I don't know how to do it differently atm.
    hCopyContents :: Integer -> Handle -> Handle -> IO ()
    hCopyContents fileSize source target = do
      contents <- BS.hGetContents source
      BS.hPut target contents
      when (odd fileSize) $
        BS.hPut target "\x0a"
      hFlush target