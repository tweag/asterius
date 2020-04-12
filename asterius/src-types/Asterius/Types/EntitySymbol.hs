{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Asterius.Types.EntitySymbol
  ( EntitySymbol,
    entityName,
    mkEntitySymbol,
  )
where

import qualified Binary as GHC
import qualified Data.ByteString as BS
import Data.Data
import Data.String
import qualified GhcPlugins as GHC

newtype EntitySymbol = EntitySymbol GHC.FastString
  deriving newtype (Eq, Ord, Show, IsString, Semigroup, Monoid, GHC.Binary)
  deriving stock (Data)

{-# INLINE entityName #-}
entityName :: EntitySymbol -> BS.ByteString
entityName (EntitySymbol k) = GHC.fastStringToByteString k

{-# INLINE mkEntitySymbol #-}
mkEntitySymbol :: BS.ByteString -> EntitySymbol
mkEntitySymbol = EntitySymbol . GHC.mkFastStringByteString
