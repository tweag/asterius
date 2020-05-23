{-# LANGUAGE OverloadedStrings #-}

import System.Environment.Blank
import System.Process (callProcess)
import qualified Data.ByteString as BS
import System.IO (IOMode(..), withFile, Handle, hFileSize, hFlush)
import Control.Monad (forM_, when)
import qualified Data.ByteString.Char8 as BSC
-- import System.Directory (doesFileExist)

-- TODOs:
-- * Add proper checks (doesFileExist, etc.)
-- * Add proper flag parsing
-- * Maybe work on a temporary file first and copy the results afterwards?
-- * Cleanup and ormolize
-- * Still unclear: should we use withBinaryFile etc. instead?

-- Some resources:
--   https://github.com/haskell/cabal/blob/master/Cabal/Distribution/Simple/Program/Ar.hs
--   https://en.wikipedia.org/wiki/Ar_(Unix)
--   https://pubs.opengroup.org/onlinepubs/007908799/xcu/ar.html
--   https://www.freebsd.org/cgi/man.cgi?query=ar&sektion=5&manpath=4.3BSD+NET%2F2

main :: IO ()
main = do
  args <- getArgs
  callProcess "ar" args -- For now just call GNU ar

-- | Get all arguments, looking through file arguments.
--
-- TODO: Adjust this function to be faithful to the behavior described in @man ar@:
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
getArgsRecursively = getArgs >>= expandAtOptions
  where
    expandAtOptions :: [String] -> IO [String]
    expandAtOptions opts = concat <$> mapM expandAtOption opts

    expandAtOption :: String -> IO [String]
    expandAtOption ('@':path) = readFile path >>= expandAtOptions . words
    expandAtOption opt = return [opt]

-----------------------------------------------------------------------------

-- | Create a library archive from a bunch of object files.
createArchive :: Bool -> FilePath -> [FilePath] -> IO ()
createArchive is_truncation_allowed arFile objFiles =
  withFile arFile WriteMode $ \ah -> do
    BS.hPut ah "!<arch>\x0a" -- ASCII Magic String (8 bytes)
    forM_ objFiles $ hPutObjFile ah
  where
    hPutObjFile :: Handle -> FilePath -> IO ()
    hPutObjFile ah filename = do
      withFile filename ReadMode $ \oh -> do
        let fileID = mkFileID filename
        fileSize <- hFileSize oh
        let bsFileSize = BSC.pack $ take 10 $ show fileSize ++ repeat ' '
        hPutFileHeader ah fileID bsFileSize
        hCopyContents fileSize oh ah
        hFlush ah -- Better safe than sorry

    -- TODO: assert that filename and filesize have the proper size.
    hPutFileHeader :: Handle -> BS.ByteString -> BS.ByteString -> IO ()
    hPutFileHeader h filename filesize = do
      BS.hPut h filename        -- File identified (16 bytes)
      BS.hPut h "0           "  -- File modification timestamp (12 bytes)
      BS.hPut h "0     "        -- Owner ID (6 bytes)
      BS.hPut h "0     "        -- Group ID (6 bytes)
      BS.hPut h "0644    "      -- File mode (8 bytes)
      BS.hPut h filesize        -- File size (10 bytes)
      BS.hPut h "\x60\x0a"      -- "Magic" ending characters (2 bytes)

    -- NOTE: length should be exactly 16 bytes
    mkFileID :: FilePath -> BS.ByteString
    mkFileID filename
      | length filename <= 16 = BSC.pack $ take 16 $ filename ++ repeat ' '
      | is_truncation_allowed = BSC.pack $ take 16 $ filename
      | otherwise = error $ "ahc-ar: " ++ filename ++ "is too long" -- TODO: Suggestion to set -T?

    -- TODO: this doesn't look great, but I don't know how to do it differently atm.
    hCopyContents :: Integer -> Handle -> Handle -> IO ()
    hCopyContents fileSize from to = do
      contents <- BS.hGetContents from
      BS.hPut to contents
      when (odd fileSize) $
        BS.hPut to "\x0a"
      hFlush to

