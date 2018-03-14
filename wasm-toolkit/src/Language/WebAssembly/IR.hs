{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-top-binds #-}

module Language.WebAssembly.IR
  ( ValueType(..)
  , FunctionType(..)
  , MemSpec
  --, Expression(..)
  ) where

import qualified Data.ByteString.Short as SBS
import Data.Hashable
import Data.Serialize
import qualified Data.Vector as V
import GHC.Generics

data ValueType
  = NoneType
  | Int32Type
  | Int64Type
  | Float32Type
  | Float64Type
  | UnreachableType
  deriving (Eq, Generic, Show)

instance Hashable ValueType

instance Serialize ValueType

data FunctionType = FunctionType
  { paramTypes :: V.Vector ValueType
  , resultType :: ValueType
  } deriving (Eq, Generic, Show)

instance Hashable a => Hashable (V.Vector a) where
  hashWithSalt s = hashWithSalt s . V.toList
  {-# INLINEABLE hashWithSalt #-}

instance Hashable FunctionType

instance Serialize a => Serialize (V.Vector a) where
  put = put . V.toList
  {-# INLINEABLE put #-}
  get = V.fromList <$> get
  {-# INLINEABLE get #-}

instance Serialize FunctionType

class MemSpec mspec where
  type Index mspec
  type MemArg mspec

data Expression mspec
  = Call { target :: SBS.ShortByteString
         , operands :: V.Vector (Expression mspec)
         , returnType :: ValueType }
  | CallImport { target :: SBS.ShortByteString
               , operands :: V.Vector (Expression mspec)
               , returnType :: ValueType }
  | CallIndirect { indirectTarget :: Expression mspec
                 , operands :: V.Vector (Expression mspec)
                 , returnType :: ValueType }
  | GetLocal { index :: Index mspec
             , returnType :: ValueType }
  | SetLocal { index :: Index mspec
             , value :: Expression mspec }
  | TeeLocal { index :: Index mspec
             , value :: Expression mspec }
  | GetGlobal { name :: SBS.ShortByteString
              , returnType :: ValueType }
