{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

import Asterius.Internals
import Asterius.Internals.Temp
import Asterius.Types
import Control.Exception
import Control.Monad
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Traversable
import Prelude hiding (IO)
import System.Directory
import System.Environment.Blank
import System.Process

objectSymbolMap ::
     FilePath -> IO (Map AsteriusEntitySymbol AsteriusModuleSymbol)
objectSymbolMap obj_path = do
  r <- tryDecodeFile obj_path
  case r of
    Left _ -> pure Map.empty
    Right m -> do
      when (currentModuleSymbol m == noModuleSymbol) $
        throwIO $ userError $ "ahc-ar: error when decoding " <> obj_path
      let f = Map.map (const $ currentModuleSymbol m)
          symbol_map =
            Map.unions
              [ f (staticsMap m)
              , f (staticsErrorMap m)
              , f (functionMap m)
              , f (functionErrorMap m)
              ]
      pure symbol_map

archiveIndex :: [FilePath] -> IO FilePath
archiveIndex obj_paths = do
  symbol_map <- mconcat <$> for obj_paths objectSymbolMap
  index_path <- temp "INDEX"
  encodeFile index_path symbol_map
  pure index_path

main :: IO ()
main = do
  (reverse -> (('@':rsp_path):archive_path:_)) <- getArgs
  obj_paths <- lines <$> readFile rsp_path
  index_path <- archiveIndex obj_paths
  new_rsp_path <- temp "ar.rsp"
  writeFile new_rsp_path $ unlines (index_path : obj_paths)
  callProcess "ar" ["-r", "-c", archive_path, '@' : new_rsp_path]
  removeFile index_path
  removeFile new_rsp_path
