{-# LANGUAGE DeriveGeneric, StandaloneDeriving, BangPatterns, CPP #-}
module GenericsBenchCache (readPackageDescriptionCache) where

import qualified Data.ByteString.Lazy                          as L
import qualified Data.ByteString.Lazy.Char8                    as LC8
import qualified Codec.Compression.GZip                        as GZip

import           Cabal24 (PackageDescription)

import           System.Directory
import           System.Exit

import           GenericsBenchTypes                            ()

#if ! MIN_VERSION_base(4,8,0)
import           Control.Applicative                           ((<$>))
#endif

readPackageDescriptionCache :: Int -> IO [PackageDescription]
readPackageDescriptionCache amount = do
  cacheExists <- doesFileExist cacheFilePath
  bs <-
    if cacheExists
      then do
        putStrLn "reading the cache file, might take a moment..."
        L.readFile cacheFilePath
      else do
        -- In older versions of this benchmark, there was machinery to
        -- regenerate the cache using the data in @~/.cabal@. Now the cache is
        -- simply stored in the repo to avoid a dependency on Cabal the library.
        putStrLn (cacheFilePath ++ " missing, aborting")
        exitFailure
  let str = LC8.unpack (GZip.decompress bs)
      pds = take amount (read str)
  -- PackageDescription doesn't implement NFData, let's force with the following line
  (length (show pds)) `seq` putStrLn "done reading the cache file"
  return pds

cacheFilePath :: String
cacheFilePath = "generics-bench.cache.gz"
