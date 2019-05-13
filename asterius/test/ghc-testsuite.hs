{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

import Asterius.JSRun.Main
import qualified Data.ByteString.Lazy as LBS
import Data.Traversable
import Language.JavaScript.Inline.Core
import System.Directory
import System.FilePath
import System.Process
import Test.Tasty
import Test.Tasty.Hspec

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

runTestCase :: TestCase -> IO (LBS.ByteString, LBS.ByteString)
runTestCase TestCase {..} = do
  _ <- readProcess "ahc-link" ["--input-hs", casePath] ""
  mod_buf <- LBS.readFile $ casePath -<.> "wasm"
  withJSSession defJSSessionOpts $ \s -> do
    i <- newAsteriusInstance s (casePath -<.> "lib.mjs") mod_buf
    hsInit s i
    hsMain s i
    hs_stdout <- hsStdOut s i
    hs_stderr <- hsStdErr s i
    pure (hs_stdout, hs_stderr)

makeTestTree :: TestCase -> IO TestTree
makeTestTree c@TestCase {..} =
  testSpec casePath $
  it casePath $ do
    (hs_stdout, hs_stderr) <- runTestCase c
    hs_stdout `shouldBe` caseStdOut
    hs_stderr `shouldBe` caseStdErr

main :: IO ()
main = do
  trees <- getTestCases >>= traverse makeTestTree
  defaultMain $ testGroup "asterius ghc-testsuite" trees
