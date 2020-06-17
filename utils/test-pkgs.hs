{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import System.Environment.Blank
import System.Exit
import System.IO
import System.IO.Unsafe
import System.Process

pooledFoldMap :: Monoid m => Int -> (a -> IO m) -> [a] -> IO m
pooledFoldMap nprocs f jobs = do
  queue <- newIORef jobs
  rets <- forM [1 .. nprocs] $ \_ -> do
    cache <- newIORef mempty
    ret <- newEmptyMVar
    let w = join $ atomicModifyIORef' queue $ \case
          (x : xs) ->
            ( xs,
              do
                r <- f x
                modifyIORef' cache (r <>)
                w
            )
          [] -> ([], pure ())
    void $
      forkFinally
        w
        ( \case
            Left (SomeException err) -> putMVar ret (throw err)
            _ -> readIORef cache >>= putMVar ret
        )
    pure ret
  mconcat <$> forM rets takeMVar

testPkg :: String -> IO [String]
testPkg pkg = do
  (c, _stdout, _stderr) <-
    readProcessWithExitCode
      "ahc-cabal"
      [ "v1-install",
        "--dry-run",
        "--minimize-conflict-set",
        "--package-db=clear",
        "--package-db=global",
        pkg
      ]
      ""
  case c of
    ExitSuccess -> do
      withMVar stdoutLock $ \_ -> putStrLn $ "[INFO] OK: " <> pkg
      pure [pkg]
    _ -> do
      withMVar stdoutLock $ \_ -> putStr _stdout
      withMVar stderrLock $ \_ -> hPutStr stderr _stderr
      pure []

main :: IO ()
main = do
  [n, pkgs_path] <- getArgs
  pkgs <- words <$> readFile pkgs_path
  pkgs_out <- sort <$> pooledFoldMap (read n) testPkg pkgs
  writeFile pkgs_path $ unlines pkgs_out

stdoutLock, stderrLock :: MVar ()
{-# NOINLINE stdoutLock #-}
stdoutLock = unsafePerformIO $ newMVar ()
{-# NOINLINE stderrLock #-}
stderrLock = unsafePerformIO $ newMVar ()
