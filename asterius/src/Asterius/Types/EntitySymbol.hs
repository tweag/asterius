{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Asterius.Types.EntitySymbol
  ( EntitySymbol,
    entityName,
    mkEntitySymbol,
  )
where

import Data.Binary
import qualified Data.ByteString as BS
import Data.Data
import Data.String
import qualified GhcPlugins as GHC

newtype EntitySymbol = EntitySymbol BS.ByteString
  deriving newtype (Eq, Ord, Show, IsString, Semigroup, Monoid)
  deriving stock (Data)

{-# INLINE entityName #-}
entityName :: EntitySymbol -> BS.ByteString
entityName (EntitySymbol k) = k

{-# INLINE mkEntitySymbol #-}
mkEntitySymbol :: BS.ByteString -> EntitySymbol
mkEntitySymbol bs
  | BS.null bs =
    EntitySymbol BS.empty
  | otherwise =
    EntitySymbol $ GHC.fastStringToByteString $ GHC.mkFastStringByteString bs

instance Binary EntitySymbol where
  {-# INLINE put #-}
  put = put . entityName
  {-# INLINE get #-}
  get = mkEntitySymbol <$> get
