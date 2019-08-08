{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
import qualified Asterius.BuildInfo as A
import qualified Asterius.FrontendPlugin as A
import Data.Foldable
import Distribution.Simple.Utils
import Distribution.Verbosity
import Language.Haskell.GHC.Toolkit.FakeGHC
import System.Console.GetOpt
import System.Environment.Blank
import System.Exit
import System.FilePath
import System.Directory
import System.IO

main :: IO ()
main = do
  bootDir <- A.getBootDir
  ahcLd <- A.getAhcLd
  args <- getArgs
  if
    | "--install-executable" `elem` args ->
        installExecutable $ parseInstallExeArgs args
    | "--run" `elem` args -> run args
    | otherwise ->
        fakeGHCMain $
          FakeGHCOptions
            A.ghc
            (bootDir </> ".boot" </> "asterius_lib")
            A.frontendPlugin
            ["-pgml" <> ahcLd]

data InstallExeArgs = InstallExeArgs
  { output       :: Maybe FilePath
  , stripProgram :: Maybe FilePath
  , inputs       :: [FilePath]
  , verbosity    :: Verbosity
  } deriving (Show)

parseInstallExeArgs :: [String] -> InstallExeArgs
parseInstallExeArgs args =
  case opt of
    (ops, inputs, _, []) ->
      foldl'
        (flip ($))
        InstallExeArgs
          { output       = Nothing
          , stripProgram = Nothing
          , inputs       = inputs
          , verbosity    = normal
          }
        ops
    (_, _, _, err_msgs) -> error $ show err_msgs
  where
    opt =
      getOpt'
        Permute
        [ Option [] ["install-executable"] (NoArg id) ""
        , Option ['o'] [] (ReqArg (\s t -> t {output = Just s}) "FILE") ""
        , Option [] ["strip-program"] (ReqArg (\s t -> t {stripProgram = Just s}) "PROGRAM") ""
        , Option ['v'] [] (NoArg (\t -> t {verbosity = verbose})) ""
        ]
        args

wexeExtension :: FilePath
wexeExtension = "wexe"

installExecutable :: InstallExeArgs -> IO ()
installExecutable InstallExeArgs
      { output = Just output
      , stripProgram
      , inputs = [input]
      , verbosity} =
  doesDirectoryExist (input <.> wexeExtension) >>= \case
    False -> do
      hPutStrLn stderr $ "No executable found to install at "
        <> (input <.> wexeExtension)
      exitFailure
    True ->
      installDirectoryContents verbosity (input <.> wexeExtension)
        (output <.> wexeExtension)
installExecutable _ = hPutStrLn stderr "Please provide an output path (`-o`) and exactly one input path"

run :: [String] -> IO ()
run args = putStrLn $ "TODO " <> show args

