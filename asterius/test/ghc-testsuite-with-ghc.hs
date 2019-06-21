module Main (main) where

import Data.Traversable
import Data.List (isPrefixOf)
import Control.Monad (forM_, when, unless)
import System.Environment (getEnvironment, unsetEnv)
import System.Directory
import System.FilePath
import System.Process (readProcessWithExitCode)
import Data.List (sort)

data TestCase = TestCase
  { casePath :: FilePath
  , stdoutPath :: FilePath
  , stderrPath :: FilePath
  , caseStdIn :: String
  } deriving (Show)

-- | Try to read a file. if file does not exist, then return empty string.
readFileNullable :: FilePath -> IO String
readFileNullable p = do
  exist <- doesFileExist p
  if exist
    then readFile p
    else pure ""

getTestCases :: IO [TestCase]
getTestCases = do
  let root = "test" </> "ghc-testsuite"
  subdirs <- sort <$> listDirectory root
  fmap concat $
    for subdirs $ \subdir -> do
      let subroot = root </> subdir
      files <- sort <$> listDirectory subroot
      let cases = map (subroot </>) $ filter ((== ".hs") . takeExtension) files
      for cases $ \c -> do
        -- | GHC has some tests that differ for 32 and 64 bit architectures. So,
        -- we first check if the 64 bit test exists. If it does, we always
        -- use it. If it does not, we use the default test (which should
        -- be the same for both architectures).
        stdout64exists <- doesFileExist (c -<.> "stdout-ws-64")
        let stdoutFile = c -<.> ("stdout" <>  if stdout64exists then "-ws-64" else "")
        stderr64exists <- doesFileExist (c -<.> "stderr-ws-64")
        let stderrFile = c -<.> ("stderr" <> if stderr64exists then "-ws-64" else "")
        stdin <- readFileNullable (c -<.> "stdin")
        pure TestCase { casePath = c, stdoutPath = stdoutFile, stderrPath = stderrFile, caseStdIn = stdin }

runTestCaseUsingGhc :: TestCase -> IO ()
runTestCaseUsingGhc testCase = do
  -- TODO: use stdin
  putStrLn $ "Running " ++ casePath testCase ++ " ..."
  let stdinString = caseStdIn testCase
  (_exitCode, stdout, stderr) <- readProcessWithExitCode "runhaskell" [casePath testCase] stdinString
  unless (null stdout) $
    writeFile (stdoutPath testCase) stdout
  unless (null stderr) $
    writeFile (stderrPath testCase) stderr

-- unset environment variables set by stack
cleanEnvironment :: IO ()
cleanEnvironment = do
  env <- getEnvironment
  forM_ env $ \(envVarName, _envVarValue) ->
    when ("HASKELL_" `isPrefixOf` envVarName || "GHC_" `isPrefixOf` envVarName) $
      unsetEnv envVarName

main :: IO ()
main = do
  cleanEnvironment
  testCases <- getTestCases
  forM_ testCases runTestCaseUsingGhc