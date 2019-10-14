{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

import Asterius.Internals.Temp
import Asterius.JSRun.Main
import Control.Exception
import qualified Data.ByteString.Lazy as LBS
import Data.Csv
import Data.IORef
import Data.List
  ( sort,
    sortOn,
  )
import Data.Maybe
import Data.Ord
import Data.Traversable
import Data.Word
import GHC.Generics
import Language.JavaScript.Inline.Core
import System.Directory
import System.Environment.Blank
import System.FilePath
import System.Process
import Test.Tasty
import Test.Tasty.HUnit

data TestCase
  = TestCase
      { casePath :: FilePath,
        caseStdIn, caseStdOut, caseStdErr :: LBS.ByteString
      }
  deriving (Show)

-- Convert a Char to a Word8
charToWord8 :: Char -> Word8
charToWord8 = toEnum . fromEnum

-- Try to read a file. if file does not exist, then return empty string.
readFileNullable :: FilePath -> IO LBS.ByteString
readFileNullable p = do
  exist <- doesFileExist p
  if exist
    then do
      bs <- LBS.readFile p
      -- Add trailing whitespace if it does not exist.
      -- The GHC testsuite also performs normalization:
      -- testsuite/driver/testlib.py
      if LBS.last bs /= charToWord8 '\n'
        then return $ LBS.snoc bs (charToWord8 '\n')
        else return bs
    else pure LBS.empty

getTestCases :: IO [TestCase]
getTestCases = do
  let root = "test" </> "ghc-testsuite"
  subdirs <- sort <$> listDirectory root
  fmap concat $ for subdirs $ \subdir -> do
    let subroot = root </> subdir
    files <- sort <$> listDirectory subroot
    let cases = map (subroot </>) $ filter ((== ".hs") . takeExtension) files
    for cases $ \c ->
      -- GHC has some tests that differ for 32 and 64 bit architectures. So,
      -- we first check if the 64 bit test exists. If it does, we always
      -- use it. If it does not, we use the default test (which should
      -- be the same for both architectures).
      do
        ws64exists <- doesFileExist (c -<.> "stdout-ws-64")
        let stdoutp = c -<.> ("stdout" <> if ws64exists then "-ws-64" else "")
        ws64exists <- doesFileExist (c -<.> "stderr-ws-64")
        let stderrp = c -<.> ("stderr" <> if ws64exists then "-ws-64" else "")
        TestCase c
          <$> readFileNullable (c -<.> "stdin")
          <*> readFileNullable stdoutp
          <*> readFileNullable stderrp

data TestOutcome
  = TestSuccess
  | TestFailure
  deriving (Eq, Show, Generic)

instance ToField TestOutcome where
  toField = toField . show

data TestRecord
  = TestRecord
      { trOutcome :: TestOutcome,
        -- | Path of the test case
        trPath :: FilePath,
        -- | If the test failed, then the error message associated to the failure.
        trErrorMessage :: String
      }
  deriving (Generic)

instance ToRecord TestRecord

instance DefaultOrdered TestRecord

instance ToNamedRecord TestRecord

runTestCase :: [String] -> IORef [TestRecord] -> TestCase -> IO ()
runTestCase l_opts tlref TestCase {..} = catch m h
  where
    h (e :: SomeException) = do
      atomicModifyIORef' tlref $ \trs ->
        ( TestRecord
            { trOutcome = TestFailure,
              trPath = casePath,
              trErrorMessage = show e
            }
            : trs,
          ()
        )
      throwIO e
    m = do
      withTempDir "" $ \tmp_dir -> do
        let tmp_case_path = tmp_dir </> takeFileName casePath
        copyFile casePath tmp_case_path
        _ <-
          readCreateProcess
            ( proc "ahc-link" $
                [ "--input-hs",
                  takeFileName tmp_case_path,
                  "--binaryen",
                  "--verbose-err"
                ]
                  <> l_opts
            )
              { cwd = Just tmp_dir,
                std_err = CreatePipe
              }
            ""
        mod_buf <- LBS.readFile $ tmp_case_path -<.> "wasm"
        withJSSession
          defJSSessionOpts
            { nodeExtraArgs =
                ["--experimental-wasm-bigint" | "--debug" `elem` l_opts]
                  <> [ "--experimental-wasm-return-call"
                       | "--tail-calls" `elem` l_opts
                     ]
            }
          $ \s -> do
            i <- newAsteriusInstance s (tmp_case_path -<.> "lib.mjs") mod_buf
            hsInit s i
            hsMain s i
            hs_stdout <- hsStdOut s i
            hs_stderr <- hsStdErr s i
            hs_stdout @?= caseStdOut
            hs_stderr @?= caseStdErr
      atomicModifyIORef' tlref $ \trs ->
        ( TestRecord
            { trOutcome = TestSuccess,
              trPath = casePath,
              trErrorMessage = ""
            }
            : trs,
          ()
        )

makeTestTree :: [String] -> IORef [TestRecord] -> TestCase -> TestTree
makeTestTree l_opts tlref c@TestCase {..} =
  testCase casePath $ runTestCase l_opts tlref c

-- save the test log to disk as a CSV file
saveTestLogToCSV :: IORef [TestRecord] -> FilePath -> IO ()
saveTestLogToCSV tlref out_basepath = do
  let out_csvpath = out_basepath <.> "csv"
  tlv_raw <- readIORef tlref
  let tlv =
        flip sortOn tlv_raw $ \TestRecord {..} -> Down $ splitDirectories trPath
  putStrLn $ "[INFO] Writing log CSV file to path: " <> out_csvpath
  LBS.writeFile out_csvpath $ encodeDefaultOrderedByName tlv

main :: IO ()
main = do
  m_opts <- getEnv "ASTERIUS_GHC_TESTSUITE_OPTIONS"
  let l_opts = maybeToList m_opts >>= words
  tlref <- newIORef mempty
  trees <- map (makeTestTree l_opts tlref) <$> getTestCases
  cwd <- getCurrentDirectory
  let out_basepath = cwd </> "test-report"
  -- Tasty throws an exception if stuff fails, so re-throw the exception
  -- in case this happens.
  defaultMain (testGroup "asterius ghc-testsuite" trees)
    `finally` saveTestLogToCSV tlref out_basepath
