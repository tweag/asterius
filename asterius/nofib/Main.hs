{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.Makefile
import qualified Data.Makefile.Parse as P
import System.Directory
import Control.Monad
import System.FilePath.Posix
import qualified Data.List as List
import Data.Text

-- | Get all the contents of a directory that are themselved directories.
getSubDirectories :: FilePath -> IO [FilePath]
getSubDirectories fp = do
  subs <- listDirectory fp
  filterM doesDirectoryExist [fp </> sub | sub <- subs]

parseAsMakefile :: FilePath -> IO (Either String Makefile)
parseAsMakefile fp = doesPathExist fp >>= \case
  True -> P.parseAsMakefile fp
  False -> pure $ Left "non-existent"

extractOpts :: Makefile -> Maybe (Text, Text, Text) -- FAST, NORM, SLOW
extractOpts makefile = do
  let assgns = [(lhs, rhs) | Assignment RecursiveAssign lhs rhs <- entries makefile]
  fast <- snd <$> List.find (\e -> fst e == "FAST_OPTS") assgns
  norm <- snd <$> List.find (\e -> fst e == "NORM_OPTS") assgns
  slow <- snd <$> List.find (\e -> fst e == "SLOW_OPTS") assgns
  return (fast, norm, slow)


main :: IO ()
main = do
  -- All the test directories
  all_test_dirs <- do
    top_test_dirs <- do
      dot <- getCurrentDirectory
      dirs <- listDirectory dot >>= filterM isTopTestDir
      return [dot </> dir | dir <- dirs]
    List.concat <$> mapM getSubDirectories top_test_dirs

  forM_ all_test_dirs $ \test_dir -> do
    print test_dir
    putStr "  "
    parseAsMakefile (test_dir </> "Makefile") >>= \case
      Left err -> putStrLn $ "errored: " ++ err
      Right mk -> putStrLn $ "succeeded: " ++ show (extractOpts mk)
  where
    isTopTestDir s = do
      bool <- doesDirectoryExist s
      return (bool && s /= "common")


--   FAST_OPTS =   500000
--   NORM_OPTS =  5000000
--   SLOW_OPTS = 25000000


