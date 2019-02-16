{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

import Asterius.Internals hiding (decodeFile, encodeFile)
import Asterius.Types
import Control.Exception
import Control.Monad
import Data.Binary
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Traversable
import Prelude hiding (IO)
import System.Directory
import System.Environment.Blank
import System.FilePath
import System.IO hiding (IO)
import System.Process

objectSymbolMap :: FilePath -> IO (Map AsteriusEntitySymbol FilePath)
objectSymbolMap obj_path = do
  r <- decodeFileOrFail obj_path
  case r of
    Left _ -> pure Map.empty
    Right m -> do
      when (currentModuleSymbol m == noModuleSymbol) $
        throwIO $ userError $ "ahc-ar: error when decoding " <> obj_path
      let syms =
            mconcat
              [ Map.keys (staticsMap m)
              , Map.keys (staticsErrorMap m)
              , Map.keys (functionMap m)
              , Map.keys (functionErrorMap m)
              ]
      pure $ Map.fromList [(sym, takeBaseName obj_path) | sym <- syms]

archiveIndex :: [FilePath] -> IO FilePath
archiveIndex obj_paths = do
  symbol_map <- mconcat <$> for obj_paths objectSymbolMap
  tmpdir <- getTemporaryDirectory
  let index_path = tmpdir </> "INDEX"
  encodeFile index_path symbol_map
  pure index_path

main :: IO ()
main = do
  (reverse -> (('@':rsp_path):archive_path:_)) <- getArgs
  obj_paths <- lines <$> readFile rsp_path
  index_path <- archiveIndex obj_paths
  tmpdir <- getTemporaryDirectory
  (new_rsp_path, new_rsp_h) <- openTempFile tmpdir "ar.rsp"
  hPutStr new_rsp_h $ unlines (index_path : obj_paths)
  hClose new_rsp_h
  callProcess "ar" ["-r", "-c", archive_path, '@' : new_rsp_path]
  removeFile index_path
  removeFile new_rsp_path
