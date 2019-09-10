{-# LANGUAGE ViewPatterns #-}

import Asterius.Internals (decodeFile)
import Asterius.Ld
import qualified Asterius.Main as Main
import Asterius.Types
import Data.List
import Data.Maybe
import Data.String
import Data.Traversable
import System.Directory
import System.FilePath
import System.Environment.Blank

parseLinkTask :: [String] -> IO LinkTask
parseLinkTask args = do
  link_libs <- fmap catMaybes $ for link_libnames $ findFile link_libdirs
  pure
    LinkTask
      { progName = prog_name
      , linkOutput = link_output
      , linkObjs = link_objs
      , linkLibs = link_libs
      , linkModule = mempty
      , debug = "--debug" `elem` args
      , gcSections = "--no-gc-sections" `notElem` args
      , binaryen = "--binaryen" `elem` args
      , verboseErr = "--verbose-err" `elem` args
      , outputIR =
          find ("--output-ir=" `isPrefixOf`) args >>= stripPrefix "--output-ir="
      , rootSymbols =
          map (AsteriusEntitySymbol . fromString) $
          str_args "--extra-root-symbol="
      , exportFunctions =
          map (AsteriusEntitySymbol . fromString) $
          str_args "--export-function="
      }
  where
    Just (stripPrefix "--prog-name=" -> Just prog_name) =
      find ("--prog-name=" `isPrefixOf`) args
    link_output = args !! succ output_i
    Just output_i = elemIndex "-o" args
    link_objs = filter ((== "o.") . take 2 . reverse) args
    link_libnames =
      map ((<> ".a") . ("lib" <>) . drop 2) $ filter ((== "-l") . take 2) args
    link_libdirs = map (drop 2) $ filter ((== "-L") . take 2) args
    str_args arg_prefix = mapMaybe (stripPrefix arg_prefix) args

main :: IO ()
main = do
  args <- getArgs
  case args of
    "-Wl,--version":_ -> putStr "LLD"
    _ -> do
      let Just ('@':rsp_path) = find ((== '@') . head) args
      rsp <- readFile rsp_path
      let rsp_args = map read $ lines rsp
      linkTask' <- parseLinkTask rsp_args
      let wexe = linkOutput linkTask' <.> "wexe"
          baseName = takeBaseName $ linkOutput linkTask'
          linkTask = linkTask' { linkOutput = wexe </> "link.out" }
      createDirectoryIfMissing False wexe
      linkExe linkTask
      let task = Main.parseTask []
      ld_result <- decodeFile $ linkOutput linkTask
      Main.ahcDistMain putStrLn task
        { Main.target = Main.Node
        , Main.inputHS = linkOutput linkTask
        , Main.inputEntryMJS = Nothing
        , Main.outputDirectory = wexe
        , Main.outputBaseName = baseName
        , Main.run = False
        , Main.bundle = True
        } ld_result
