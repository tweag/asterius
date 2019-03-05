{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Asterius.Internals
import Asterius.Internals.Temp
import Asterius.Types
import Data.Either
import Data.Traversable
import Prelude hiding (IO)
import System.Directory
import System.Environment.Blank
import System.Process

main :: IO ()
main = do
  (reverse -> (('@':rsp_path):archive_path:_)) <- getArgs
  obj_paths <- lines <$> readFile rsp_path
  (store :: AsteriusModule) <- mconcat . rights <$> for obj_paths tryDecodeFile
  store_path <- temp "MODULE"
  encodeFile store_path store
  new_rsp_path <- temp "ar.rsp"
  writeFile new_rsp_path $ unlines [store_path]
  callProcess "ar" ["-r", "-c", archive_path, '@' : new_rsp_path]
  removeFile store_path
  removeFile new_rsp_path
