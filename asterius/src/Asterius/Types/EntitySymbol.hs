{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Asterius.Types.EntitySymbol
  ( EntitySymbol,
    entityName,
    mkEntitySymbol,
    getKeyES,
  )
where

import qualified Binary as GHC
import Control.DeepSeq
import qualified Data.ByteString as BS
import Data.Data
import Data.String
import qualified GhcPlugins as GHC
import qualified Unique as GHC

newtype EntitySymbol = EntitySymbol GHC.FastString
  deriving newtype (Eq, Ord, Show, IsString, Semigroup, Monoid, GHC.Binary, GHC.Uniquable)
  deriving stock (Data)

instance NFData EntitySymbol where
  rnf = rwhnf

-- | Convert an 'EntitySymbol' to a 'BS.ByteString'.
{-# INLINE entityName #-}
entityName :: EntitySymbol -> BS.ByteString
entityName (EntitySymbol k) = GHC.fastStringToByteString k

-- | Create an 'EntitySymbol' from a 'BS.ByteString'.
{-# INLINE mkEntitySymbol #-}
mkEntitySymbol :: BS.ByteString -> EntitySymbol
mkEntitySymbol = EntitySymbol . GHC.mkFastStringByteString

-- | Compute the key ('Int') of the 'GHC.Unique' of an 'EntitySymbol'.
{-# INLINE getKeyES #-}
getKeyES :: EntitySymbol -> Int
getKeyES = GHC.getKey . GHC.getUnique
