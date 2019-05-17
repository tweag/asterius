{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DeriveGeneric #-}

import Asterius.JSRun.Main
import qualified Data.ByteString.Lazy as LBS
import Data.Traversable
import Language.JavaScript.Inline.Core
import System.Directory
import System.FilePath
import System.Process
import Test.Tasty
import Test.Tasty.Hspec
import Control.Exception
import Data.IORef
import Data.Aeson
import GHC.Generics

data TestCase = TestCase
  { casePath :: FilePath
  , caseStdIn, caseStdOut, caseStdErr :: LBS.ByteString
  } deriving (Show)

readFileNullable :: FilePath -> IO LBS.ByteString
readFileNullable p = do
  exist <- doesFileExist p
  if exist
    then LBS.readFile p
    else pure LBS.empty

getTestCases :: IO [TestCase]
getTestCases = do
  let root = "test" </> "ghc-testsuite"
  subdirs <- listDirectory root
  fmap concat $
    for subdirs $ \subdir -> do
      let subroot = root </> subdir
      files <- listDirectory subroot
      let cases = map (subroot </>) $ filter ((== ".hs") . takeExtension) files
      for cases $ \c ->
        TestCase c <$> readFileNullable (c -<.> "stdin") <*>
        readFileNullable (c -<.> "stdout") <*>
        readFileNullable (c -<.> "stderr")




data TestOutcome = TestSuccess | TestFailure deriving(Eq, Show, Generic)

instance ToJSON TestOutcome where


data TestRecord = TestRecord
  { trOutcome :: !TestOutcome
  , trPath :: !FilePath -- ^ Path of the test case
  , trErrorMessage :: !String -- ^ If the test failed, then the error message associated to the failure.
  } deriving(Generic)

instance ToJSON TestRecord where




-- | Log of tests that have run
newtype TestLog = TestLog [TestRecord] deriving(Semigroup, Monoid, Generic)

instance ToJSON TestLog where

-- | Append a value to the test log in safe way when we have multiple threads
consTestLog :: TestRecord -> IORef TestLog -> IO ()
consTestLog tr tlref = modifyIORef' tlref (\(TestLog tl) -> TestLog $ tr:tl)

logTestFailure :: IORef TestLog -- ^ Reference to the test log
  -> FilePath -- ^ Path of the test
  -> SomeException -- ^ Exception thrown by the test case
  -> IO ()
logTestFailure tl casePath e =
  let r = TestRecord { trOutcome=TestFailure
                     , trPath = casePath
                     , trErrorMessage = show e
                     }
  in consTestLog r tl


logTestSuccess :: IORef TestLog -- ^ Reference to the test log
  -> FilePath -- ^ Path of the test
  -> IO ()
logTestSuccess tl casePath =
  let r = TestRecord { trOutcome=TestSuccess
                     , trPath = casePath
                     , trErrorMessage = ""
                     }
  in consTestLog r tl

runTestCase :: IORef TestLog -> TestCase -> IO (LBS.ByteString, LBS.ByteString)
runTestCase tl TestCase {..} = do
  _ <- readProcess "ahc-link" ["--input-hs", casePath, "--binaryen"] ""
  mod_buf <- LBS.readFile $ casePath -<.> "wasm"
  withJSSession defJSSessionOpts $ \s -> do
    i <- newAsteriusInstance s (casePath -<.> "lib.mjs") mod_buf
    hsInit s i
    outcome <- (hsMain s i *> logTestSuccess tl casePath *> pure TestSuccess) `catch`
        (\e -> logTestFailure tl casePath e *> pure TestFailure)
    outcome `shouldBe` TestSuccess -- | Make sure that we suceeded.
    hs_stdout <- hsStdOut s i
    hs_stderr <- hsStdErr s i
    pure (hs_stdout, hs_stderr)


makeTestTree :: IORef TestLog -> TestCase -> IO TestTree
makeTestTree tl c@TestCase {..} =
  testSpec casePath $
  it casePath $ do
    (hs_stdout, hs_stderr) <- runTestCase tl c
    hs_stdout `shouldBe` caseStdOut
    hs_stderr `shouldBe` caseStdErr


-- | save the test log to disk
saveTestLogToDisk :: IORef TestLog -> FilePath -> IO ()
saveTestLogToDisk tl out_path = do
      putStrLn $ "[INFO] Writing log file to path: " <> out_path
      tlv <- readIORef tl
      encodeFile out_path tlv



main :: IO ()
main = do
  tl <- newIORef mempty
  trees <- getTestCases >>= traverse (makeTestTree tl)
  let treesTest = take 50 trees

  -- | Path where the JSON is dumped
  let out_path = "test-report.json"

  -- | Tasty throws an exception if stuff fails, so re-throw the exception
  -- | in case this happens.
  (defaultMain $ testGroup "asterius ghc-testsuite" treesTest)
    `finally` (saveTestLogToDisk tl out_path)


