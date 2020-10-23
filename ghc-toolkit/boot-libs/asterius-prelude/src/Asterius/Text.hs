module Asterius.Text
  ( textFromJSString,
    textToJSString,
  )
where

import Asterius.Magic
import Asterius.Types
import Asterius.UTF8
import Control.Exception
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Foreign

{-# INLINEABLE textFromJSString #-}
textFromJSString :: JSString -> T.Text
textFromJSString v = accursedUnutterablePerformIO $ do
  bs <- utf8FromJSString v
  BS.unsafeUseAsCString bs $ \p -> do
    r <- evaluate $ T.decodeUtf8 bs
    free p
    pure r

{-# INLINEABLE textToJSString #-}
textToJSString :: T.Text -> JSString
textToJSString =
  accursedUnutterablePerformIO . utf8ToJSString . LT.encodeUtf8 . LT.fromStrict
