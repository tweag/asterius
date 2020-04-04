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

newtype EntitySymbol = EntitySymbol GHC.FastString
  deriving newtype (Eq, Ord, Show, IsString, Semigroup, Monoid)
  deriving stock (Data)

{-# INLINE entityName #-}
entityName :: EntitySymbol -> BS.ByteString
entityName (EntitySymbol sym) = GHC.fastStringToByteString sym

{-# INLINE mkEntitySymbol #-}
mkEntitySymbol :: BS.ByteString -> EntitySymbol
mkEntitySymbol = EntitySymbol . GHC.mkFastStringByteString

instance Binary EntitySymbol where
  {-# INLINE put #-}
  put = put . entityName
  {-# INLINE get #-}
  get = mkEntitySymbol <$> get
