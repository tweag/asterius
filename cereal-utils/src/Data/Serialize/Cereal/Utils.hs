module Data.Serialize.Cereal.Utils
  ( encodeFile
  , decodeFile
  ) where

import qualified Data.ByteString as BS
import Data.Serialize

{-# INLINE encodeFile #-}
encodeFile :: Serialize a => FilePath -> a -> IO ()
encodeFile p = BS.writeFile p . encode

{-# INLINE decodeFile #-}
decodeFile :: Serialize a => FilePath -> IO a
decodeFile p = do
  r <- decode <$> BS.readFile p
  case r of
    Left err -> fail err
    Right a -> pure a
