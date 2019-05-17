{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}


import Asterius.JSRun.Main
import qualified Data.ByteString.Lazy as LBS
import Data.Traversable
import Control.Applicative
import Control.Monad (when)
import Language.JavaScript.Inline.Core
import System.Directory
import System.FilePath
import System.Process
import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.ConsoleReporter
import Data.Monoid (Any(..))
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Test.Tasty.Hspec
import Test.Tasty.Runners
import Test.Tasty.Options

import Control.Exception
import Data.IORef
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import GHC.Generics

-- Much of the code is shamelessly stolen from:
-- http://hackage.haskell.org/package/tasty-1.2.2/docs/src/Test.Tasty.Ingredients.ConsoleReporter.html#consoleTestReporter
--
-- We need to ask them to open up the internals of the repo.
-- TODO: send a PR to them asking them to open up the repo.


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
                     , trErrorMessage = displayException e
                     }
  in consTestLog r tl


-- | What happened when you tried to run the test
data RunOutcome = RunSuccess | RunFailure String deriving(Eq)


-- [Note: RunOutcome's Show instance]
-- | Have the Show instance print the exception after the separator  so we can
-- | strip out the separator in the printer
-- | This way, our custom ingredient can still serialize all the information
-- | that comes after the ``, but when we print, we strip out the leading `:`
-- | and all text that follows it.
separator :: Char
separator = 'γ'

instance Show RunOutcome where
  show (RunSuccess) = "RunSuccess"
  show (RunFailure e) = "RunFailure" <> [separator] <> (show e)

instance ToJSON RunOutcome where
    toJSON RunSuccess = toJSON . show $ RunSuccess
    toJSON (RunFailure f) = toJSON $ "RunFailure(" <> show f <> ")"

logTestSuccess :: IORef TestLog -- ^ Reference to the test log
  -> FilePath -- ^ Path of the test
  -> IO ()
logTestSuccess tl casePath =
  let r = TestRecord { trOutcome=TestSuccess
                     , trPath = casePath
                     , trErrorMessage = ""
                     }
  in consTestLog r tl

runTestCase :: IORef TestLog -> TestCase -> IO ()
runTestCase tl TestCase {..} = do
  _ <- readProcess "ahc-link" ["--input-hs", casePath, "--binaryen"] ""
  mod_buf <- LBS.readFile $ casePath -<.> "wasm"
  withJSSession defJSSessionOpts $ \s -> do
    i <- newAsteriusInstance s (casePath -<.> "lib.mjs") mod_buf
    hsInit s i
    -- | Try to run main. If we throw an exception, return a
    -- RunFailure with the error message.
    ro <- (hsMain s i *> pure RunSuccess)
      `catch` (\(e :: SomeException) -> pure . RunFailure. show $ e)
    -- | Check that the run succeeded. If it did not, report a failing
    -- test case
    ro `shouldBe` RunSuccess

    -- | If the run succeded, now compare outputs.
    hs_stdout <- hsStdOut s i
    hs_stderr <- hsStdErr s i

    hs_stdout `shouldBe` caseStdOut
    hs_stderr `shouldBe` caseStdErr


-- | @cheng: Why is this called `makeTestTree`? should it not be called
-- runTestTree?
makeTestTree :: IORef TestLog -> TestCase -> IO TestTree
makeTestTree tl c@TestCase {..} =
  testSpec casePath $
    it casePath $ runTestCase tl c


-- | save the test log to disk
saveTestLogToDisk :: IORef TestLog -> FilePath -> IO ()
saveTestLogToDisk tl out_path = do
      putStrLn $ "[INFO] Writing log file to path: " <> out_path
      tlv <- readIORef tl
      LBS.writeFile out_path (encodePretty tlv)


ro :: Test.Tasty.Runners.Result -> Outcome
ro = resultOutcome


-- | Prune the description of the test result to be legible for rendering.
-- | See [Note: RunOutcome's Show instance]
resultPruneDescription :: Test.Tasty.Runners.Result -> Test.Tasty.Runners.Result
resultPruneDescription Result{..} =
   Result{resultDescription=takeWhile (/= separator) resultDescription, ..}

-- TestReporter [OptionDescription] (OptionSet -> TestTree -> Maybe (StatusMap -> IO (Time -> IO Bool)))
consoleOutput ::  IORef TestLog -> Bool -> TestOutput -> StatusMap -> IO ()
consoleOutput tlref colors toutput smap =
  getTraversal . fst $ foldTestOutput foldTest foldHeading toutput smap
  where
    foldTest _name printName getResult printResult =
      ( Traversal $ do
          printName :: IO ()
          r <- getResult
          printResult . resultPruneDescription $ r
          let tr = if resultSuccessful r
              then TestRecord TestSuccess _name ""
              else TestRecord TestFailure _name (resultDescription r)
          consTestLog tr tlref

      , Any True)
    foldHeading _name printHeading (printBody, Any nonempty) =
      ( Traversal $ do
          when nonempty $ do printHeading :: IO (); getTraversal printBody
      , Any nonempty
      )


getResultFromTVar :: TVar Status -> IO Test.Tasty.Runners.Result
getResultFromTVar var =
  atomically $ do
    status <- readTVar var
    case status of
      Done r -> return r
      _ -> retry

computeStatistics :: StatusMap -> IO Statistics
computeStatistics = getApp . foldMap (\var -> Ap $
  (\r -> Statistics 1 (if resultSuccessful r then 0 else 1))
    <$> getResultFromTVar var)


serializeToDisk :: IORef TestLog -> Ingredient
serializeToDisk tlref = TestReporter [] $
  \opts tree -> Just $ \smap ->
  let
    NumThreads numThreads = lookupOption opts
    toutput = let ?colors = True in buildTestOutput opts tree
  in do
    consoleOutput tlref True toutput smap
    return $ \time -> do
      stats <- computeStatistics smap
      let ?colors=True -- passed as implicit parameter
      printStatistics stats time
      return $ statFailures stats == 0




main :: IO ()
main = do
  tl <- newIORef mempty
  trees <- getTestCases >>= traverse (makeTestTree tl)
  let treesTest = take 50 trees

  -- | Path where the JSON is dumped
  let out_path = "test-report.json"

  -- | Tasty throws an exception if stuff fails, so re-throw the exception
  -- | in case this happens.
  (defaultMainWithIngredients [serializeToDisk tl] $ testGroup "asterius ghc-testsuite" treesTest)
    `finally` (saveTestLogToDisk tl out_path)


