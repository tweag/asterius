{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

import Asterius.JSRun.Main
import Control.Arrow ((&&&))
import Control.Exception
import Control.Monad (when)
import qualified Data.ByteString.Lazy as LBS
import Data.Csv
import Data.IORef
import Data.List (sort)
import Data.Maybe
import Data.Monoid (All(..), Any(..))
import Data.Traversable
import Data.Typeable
import Data.Word
import GHC.Generics
import Language.JavaScript.Inline.Core
import Options.Applicative
import System.Console.ANSI (hSupportsANSIColor)
import System.Directory
import System.Environment.Blank
import System.FilePath
import System.IO (stdout)
import System.Process
import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.ConsoleReporter
import Test.Tasty.Options
import Test.Tasty.Runners
import Text.Regex.TDFA

-- Much of the code is shamelessly stolen from:
-- http://hackage.haskell.org/package/tasty-1.2.2/docs/src/Test.Tasty.Ingredients.ConsoleReporter.html#consoleTestReporter
data TestCase = TestCase
  { casePath :: FilePath
  , caseStdIn, caseStdOut, caseStdErr :: LBS.ByteString
  } deriving (Show)

-- | Convert a Char to a Word8
charToWord8 :: Char -> Word8
charToWord8 = toEnum . fromEnum

-- | Try to read a file. if file does not exist, then return empty string.
readFileNullable :: FilePath -> IO LBS.ByteString
readFileNullable p = do
  exist <- doesFileExist p
  if exist
    then do
       bs <- LBS.readFile p
       -- | Add trailing whitespace if it does not exist.
       -- | The GHC testsuite also performs normalization:
       -- | testsuite/driver/testlib.py
       if LBS.last bs /= charToWord8 '\n'
       then return $ LBS.snoc bs (charToWord8 '\n')
       else return bs

    else pure LBS.empty

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
        ws64exists <- doesFileExist (c -<.> "stdout-ws-64")
        let stdoutp = c -<.> ("stdout" <>  if ws64exists then "-ws-64" else "")


        ws64exists <- doesFileExist (c -<.> "stderr-ws-64")
        let stderrp = c -<.> ("stderr" <> if ws64exists then "-ws-64" else "")

        TestCase c <$> readFileNullable (c -<.> "stdin") <*>
          readFileNullable stdoutp <*>
          readFileNullable stderrp




data TestOutcome = TestSuccess | TestFailure deriving(Eq, Show, Generic)
instance ToField TestOutcome where
  toField = toField . show


data TestRecord = TestRecord
  { trOutcome :: !TestOutcome
  , trPath :: !FilePath -- ^ Path of the test case
  , trErrorMessage :: !String -- ^ If the test failed, then the error message associated to the failure.
  } deriving(Generic)

instance ToRecord TestRecord where
instance DefaultOrdered TestRecord where
instance ToNamedRecord TestRecord where



-- | Log of tests that have run
newtype TestLog = TestLog { unTestLog :: [TestRecord] } deriving(Semigroup, Monoid, Generic)

atomicModifyIORef'_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef'_ r f = atomicModifyIORef' r $ f &&& const ()


-- | Append a value to the test log in safe way when we have multiple threads
consTestLog :: TestRecord -> IORef TestLog -> IO ()
consTestLog tr tlref = atomicModifyIORef'_ tlref (\(TestLog tl) -> TestLog $ tr:tl)



-- [Note: Abusing Tasty APIs to get readable console logs]
-- | Have the Show instance print the exception after the separator  so we can
-- | strip out the separator in the printer
-- | This way, our custom ingredient can still serialize all the information
-- | that comes after the ``, but when we print, we strip out the leading
-- | separator and all text that follows it.
separator :: Char
separator = 'γ'


-- | What happened when we tried to run the test
data RunOutcome = RunSuccess | RunFailure String deriving(Eq)

instance Show RunOutcome where
  show (RunSuccess) = "RunSuccess"
  show (RunFailure e) = "RunFailure" <> [separator] <> e



-- | What happened when we tried to compile the test
data CompileOutcome = CompileFailure String | CompileSuccess JSVal  deriving(Eq)

-- | Test if the compile outcome was true or not.
isCompileSuccess :: CompileOutcome -> Bool
isCompileSuccess (CompileSuccess _) = True
isCompileSuccess _ = False

instance Show CompileOutcome where
  show (CompileSuccess _) = show "CompileSuccess "
  show (CompileFailure e) = "CompileFailure" <> [separator] <> e

runTestCase :: TestCase -> IO ()
runTestCase TestCase {..} = do
  m_opts <- getEnv "ASTERIUS_GHC_TESTSUITE_OPTIONS"
  _ <-
    readCreateProcess
      (proc "ahc-link" $
       ["--input-hs", takeFileName casePath, "--binaryen", "--verbose-err"] <>
       (maybeToList m_opts >>= words))
        {cwd = Just $ takeDirectory casePath}
      ""
  mod_buf <- LBS.readFile $ casePath -<.> "wasm"
  withJSSession defJSSessionOpts $ \s -> do
    -- | Try to compile and setup the program. If we throw an exception,
    -- return a CompileFailure with the error message
    co <-
        (do
          i <- newAsteriusInstance s (casePath -<.> "lib.mjs") mod_buf
          hsInit s i
          pure (CompileSuccess i))
            `catch` (\(e :: SomeException) -> pure . CompileFailure . show $ e)
    co `shouldSatisfy` isCompileSuccess

    let CompileSuccess i = co

    -- | Try to run main. If we throw an exception, return a
    -- RunFailure with the error message.
    ro <- (hsMain s i *> pure RunSuccess)
      `catch` (\(e :: SomeException) -> pure . RunFailure . show $ e)
    -- | Check that the run succeeded. If it did not, report a failing
    -- test case
    ro `shouldBe` RunSuccess

    -- | If the run succeded, now compare outputs.
    hs_stdout <- hsStdOut s i
    hs_stderr <- hsStdErr s i

    hs_stdout `shouldBe` caseStdOut
    hs_stderr `shouldBe` caseStdErr


makeTestTree :: TestCase -> IO TestTree
makeTestTree c@TestCase {..} =
  testSpec casePath $
    it casePath $ runTestCase  c


-- | save the test log to disk as a CSV file
saveTestLogToCSV :: IORef TestLog -> FilePath -> IO ()
saveTestLogToCSV tlref out_basepath = do
  let out_csvpath = out_basepath <.> "csv"
  tlv <- readIORef tlref
  putStrLn $ "[INFO] Writing log CSV file to path: " <> out_csvpath
  LBS.writeFile out_csvpath (encodeDefaultOrderedByName . unTestLog $ tlv)

-- | Prune the description of the test result to be legible for rendering.
-- | See [Note: Abusing Tasty APIs to get readable console logs]
resultPruneDescription :: Test.Tasty.Runners.Result -> Test.Tasty.Runners.Result
resultPruneDescription Result{..} =
   Result{resultDescription=takeWhile (/= separator) resultDescription, ..}

-- TestReporter [OptionDescription] (OptionSet -> TestTree -> Maybe (StatusMap -> IO (Time -> IO Bool)))
consoleOutput ::  IORef TestLog -> TestOutput -> StatusMap -> IO ()
consoleOutput tlref toutput smap =
  getTraversal . fst $ foldTestOutput foldTest foldHeading toutput smap
  where
    foldTest _name printName getResult printResult =
      ( Traversal $ do
          printName :: IO ()
          r <- getResult
          _ <- printResult . resultPruneDescription $ r
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

-- | Filter the test tree according to a predicate
filterTestTree :: (TestName -> Bool) -> TestTree -> TestTree
filterTestTree f tt =
  TestGroup "***filtered***" $ [go tt]
  where
    -- go :: TestTree -> TestTree
    go (SingleTest name t) =
      if (f name)
        then SingleTest (name <> "-ENABLED") t
        else TestGroup (name <> "-DISABLED") []
    go (TestGroup name tt) =
      TestGroup (name <> "-FILTERED") (map go tt)
    go _ = error $ "unknown test tree type"

-- | Option that describes whether a test file with
-- white & black lists has been provided. Modelled after
-- TestPattern
newtype PatternFilePath = PatternFilePath (Maybe FilePath)
  deriving(Show, Eq)

noFile :: PatternFilePath
noFile = PatternFilePath Nothing


instance IsOption PatternFilePath where
  defaultValue = noFile
  parseValue = Just . PatternFilePath . Just
  optionName = return "testfile"
  optionHelp = return "selects tests that match the WHITELIST/BLACKLIST patterns from file"
  optionCLParser = mkOptionCLParser (short 'l' <> metavar "FILE")


-- | Pattern can be a whitelist or a blacklist
data Pat = PatWhite String | PatBlack String
type Pats = [Pat]

-- | Filter a string against a given pattern
patFilter :: Pat -> String -> Bool
patFilter (PatWhite p) s = s =~ p
patFilter (PatBlack p) s = not $ (s =~ p)

-- | Filter a string against *all* patterns
patsFilter :: Pats -> TestName -> Bool
patsFilter ps s = getAll . mconcat . map (\p -> All (patFilter p s)) $ ps

-- | Parse a file containing patterns into a list of pattenrs
-- !xx -> blacklist xx
-- yy -> whitelist yy
parsePatternFile :: String -> Pats
parsePatternFile s =
  map (\(x:xs) -> if x == '!' then PatBlack xs else PatWhite (x:xs)) .
  -- | filter comments
  filter (\(x:xs) ->  x /= '#') .
  -- | remove empty lines
  filter (not . null) .
  lines $ s

serializeToDisk :: IORef TestLog -> Ingredient
serializeToDisk tlref =
  TestManager [Test.Tasty.Options.Option (Proxy :: Proxy PatternFilePath) ] $
      \opts tree -> Just $ do
          let (PatternFilePath patf) = lookupOption opts
          treeFiltered <- case patf of
                      Nothing -> return $ tree
                      Just fpath -> do
                           abspath <- makeAbsolute fpath
                           pats <- parsePatternFile <$> readFile abspath
                           return $ filterTestTree (patsFilter pats) tree
          launchTestTree opts treeFiltered $ \smap -> do
            isTermColor <- hSupportsANSIColor stdout
            let ?colors = isTermColor
            -- let toutput = let ?colors = isTermColor in buildTestOutput opts tree
            let toutput = buildTestOutput opts treeFiltered
            consoleOutput tlref toutput smap
            return $ \time -> do
              stats <- computeStatistics smap
              printStatistics stats time
              return $ statFailures stats == 0

main :: IO ()
main = do
    tlref <- newIORef mempty
    trees <- getTestCases >>= traverse makeTestTree

    cwd <- getCurrentDirectory
    let out_basepath = cwd </> "test-report"

    -- | Tasty throws an exception if stuff fails, so re-throw the exception
    -- | in case this happens.
    (defaultMainWithIngredients [serializeToDisk tlref] $ testGroup "asterius ghc-testsuite" trees)
      `finally` (saveTestLogToCSV tlref out_basepath)
