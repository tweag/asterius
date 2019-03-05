{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

import Asterius.Internals
import Asterius.Internals.Temp
import Asterius.Types
import qualified Data.ByteString.Lazy as LBS
import Data.Either
import Data.IntMap.Strict (IntMap)
import Data.Map.Lazy (Map)
import Data.Traversable
import Prelude hiding (IO)
import System.Directory
import System.Environment.Blank
import System.Process

type UnsafeAsteriusModule
   = ( Map AsteriusEntitySymbol LBS.ByteString
     , Map AsteriusEntitySymbol LBS.ByteString
     , Map AsteriusEntitySymbol LBS.ByteString
     , Map AsteriusEntitySymbol LBS.ByteString
     , Map AsteriusModuleSymbol (IntMap FFIImportDecl)
     , Map AsteriusModuleSymbol (Map AsteriusEntitySymbol FFIExportDecl))

appendUnsafeAsteriusModule ::
     UnsafeAsteriusModule -> UnsafeAsteriusModule -> UnsafeAsteriusModule
appendUnsafeAsteriusModule (x0, x1, x2, x3, x4, x5) (y0, y1, y2, y3, y4, y5) =
  (x0 <> y0, x1 <> y1, x2 <> y2, x3 <> y3, x4 <> y4, x5 <> y5)

main :: IO ()
main = do
  (reverse -> (('@':rsp_path):archive_path:_)) <- getArgs
  obj_paths <- lines <$> readFile rsp_path
  store <-
    foldr1 appendUnsafeAsteriusModule . rights <$> for obj_paths tryDecodeFile
  store_path <- temp "MODULE"
  encodeFile store_path store
  new_rsp_path <- temp "ar.rsp"
  writeFile new_rsp_path $ unlines [store_path]
  callProcess "ar" ["-r", "-c", archive_path, '@' : new_rsp_path]
  removeFile store_path
  removeFile new_rsp_path
