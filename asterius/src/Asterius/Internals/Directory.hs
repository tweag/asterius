{-# LANGUAGE TupleSections #-}

module Asterius.Internals.Directory
  ( listFilesRecursive
  ) where

import Data.Foldable
import Data.Traversable
import System.Directory
import System.FilePath

listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive = w []
  where
    w acc bp = do
      fds <- map (normalise . (bp </>)) <$> listDirectory bp
      tagged_fds <- for fds $ \fd -> (fd, ) <$> doesFileExist fd
      foldrM
        (\(fd, is_f) acc' ->
           if is_f
             then pure (fd : acc')
             else w acc' fd)
        acc
        tagged_fds
