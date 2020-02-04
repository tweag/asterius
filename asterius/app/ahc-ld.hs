{-# LANGUAGE ViewPatterns #-}

import Asterius.Ld
import Asterius.Types
import Data.List
import Data.Maybe
import Data.String
import Data.Traversable
import System.Directory
import System.Environment.Blank

parseLinkTask :: [String] -> IO LinkTask
parseLinkTask args = do
  link_libs <- fmap catMaybes $ for link_libnames $ findFile link_libdirs
  pure LinkTask
    { progName = prog_name,
      linkOutput = link_output,
      linkObjs = link_objs,
      linkLibs = link_libs,
      linkModule = mempty,
      debug = "--debug" `elem` args,
      gcSections = "--no-gc-sections" `notElem` args,
      verboseErr = "--verbose-err" `elem` args,
      outputIR =
        find ("--output-ir=" `isPrefixOf`) args
          >>= stripPrefix "--output-ir=",
      rootSymbols =
        map (AsteriusEntitySymbol . fromString) $
          str_args "--extra-root-symbol=",
      exportFunctions =
        map (AsteriusEntitySymbol . fromString) $
          str_args "--export-function="
    }
  where
    prog_name
      | Just (stripPrefix "--prog-name=" -> Just v) <-
          find
            ("--prog-name=" `isPrefixOf`)
            args =
        v
      | otherwise =
        ""
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
    "-Wl,--version" : _ -> putStr "LLD"
    _ -> do
      let Just ('@' : rsp_path) = find ((== '@') . head) args
      rsp <- readFile rsp_path
      let rsp_args = map read $ lines rsp
      task <- parseLinkTask rsp_args
      ignore <- isJust <$> getEnv "ASTERIUS_AHC_LD_IGNORE"
      if ignore then writeFile "" (linkOutput task) else linkExe task
