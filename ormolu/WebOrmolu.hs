{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WebOrmolu
  ( webOrmolu,
  )
where

import Asterius.Text
import Asterius.Types
import Control.Exception
import Ormolu

webOrmolu :: JSString -> IO JSString
webOrmolu js_str = do
  r <- try $ textToJSString <$> ormolu defaultConfig "" (fromJSString js_str)
  case r of
    Left (err :: SomeException) -> pure $ toJSString $ show err
    Right js_result -> pure js_result

foreign export javascript "webOrmolu" webOrmolu :: JSString -> IO JSString
