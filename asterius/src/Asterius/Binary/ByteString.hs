module Asterius.Binary.ByteString
  ( tryGetBS,
  )
where

import Asterius.Binary.UserData
import qualified Binary as GHC
import Control.Exception
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import Foreign.Ptr

binHandleFromBS :: BS.ByteString -> IO GHC.BinHandle
binHandleFromBS bs = BS.unsafeUseAsCStringLen bs $ \(src_p, l) -> do
  bh <- GHC.openBinMem l
  GHC.withBinBuffer bh $ flip BS.unsafeUseAsCStringLen $ \(dest_p, _) ->
    BS.memcpy (castPtr dest_p) (castPtr src_p) l
  pure bh

getBS :: GHC.Binary a => BS.ByteString -> IO a
getBS bs = do
  bh <- binHandleFromBS bs
  getWithUserData bh

tryGetBS :: GHC.Binary a => BS.ByteString -> IO (Either SomeException a)
tryGetBS = try . getBS
