{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Types.EntitySymbol
  ( EntitySymbol,
    entityName,
    mkEntitySymbol,
    stripWrapperSuffix,
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
entityName (EntitySymbol k) = GHC.bytesFS k

-- | Create an 'EntitySymbol' from a 'BS.ByteString'.
{-# INLINE mkEntitySymbol #-}
mkEntitySymbol :: BS.ByteString -> EntitySymbol
mkEntitySymbol = EntitySymbol . GHC.mkFastStringByteString

-- | Strip the suffix @_wrapper@ from an 'EntitySymbol'.
{-# INLINE stripWrapperSuffix #-}
stripWrapperSuffix :: EntitySymbol -> Maybe EntitySymbol
stripWrapperSuffix sym =
  mkEntitySymbol <$> BS.stripSuffix "_wrapper" (entityName sym)

-- | Compute the key ('Int') of the 'GHC.Unique' of an 'EntitySymbol'.
{-# INLINE getKeyES #-}
getKeyES :: EntitySymbol -> Int
getKeyES = GHC.getKey . GHC.getUnique
