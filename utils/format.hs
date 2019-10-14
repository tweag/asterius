{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Concurrent
import Control.Concurrent.Async
import Data.Function
import Data.List
import System.Directory.Extra
import System.Environment
import System.FilePath
import Text.Show.Pretty

main :: IO ()
main = do
  bp <- getEnv "FORMAT_PATH"
  fs' <- listFilesRecursive bp
  q <- newMVar $ filter ("-ast" `isSuffixOf`) fs'
  n <- read <$> getEnv "FORMAT_N"
  replicateConcurrently_ n $ fix $ \w -> do
    mf <- modifyMVar q $ \case
      f : r -> pure (r, Just f)
      [] -> pure ([], Nothing)
    case mf of
      Just f -> do
        s' <- readFile f
        writeFile (f <.> "pretty") $ case parseValue s' of
          Just v -> valToStr v
          _ -> s'
        removeFile f
        w
      _ -> pure ()
