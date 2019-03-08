{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

import Ar
import Asterius.Internals
import Asterius.Types
import Data.Binary (encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Either
import Data.Map.Lazy (Map)
import Data.Traversable
import Prelude hiding (IO)
import System.Environment.Blank

type UnsafeAsteriusModule
   = ( Map AsteriusEntitySymbol LBS.ByteString
     , Map AsteriusEntitySymbol LBS.ByteString
     , Map AsteriusEntitySymbol LBS.ByteString
     , Map AsteriusEntitySymbol LBS.ByteString
     , Map AsteriusEntitySymbol LBS.ByteString
     , Map AsteriusEntitySymbol LBS.ByteString)

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
  let buf = LBS.toStrict $ encode store
  writeGNUAr archive_path $
    Archive
      [ ArchiveEntry
          { filename = "MODULE"
          , filetime = 0
          , fileown = 0
          , filegrp = 0
          , filemode = 0
          , filesize = BS.length buf
          , filedata = buf
          }
      ]
