{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.WebAssembly.IR
  ( SymbolSpec(..)
  , ConstraintSymbolSpec
  , SerializeSymbolSpec
  , Module(..)
  , Static(..)
  , StaticElement(..)
  , Function(..)
  , Block(..)
  , Expression(..)
  , Branch(..)
  ) where

import qualified Data.ByteString.Short as SBS
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Kind
import Data.Serialize
import qualified Data.Vector as V
import Data.Word
import GHC.Generics
import Language.WebAssembly.Internals ()

class SymbolSpec spec where
  type StaticSymbol spec
  type FunctionSymbol spec
  type BlockSymbol spec

type ConstraintSymbolSpec (c :: Type -> Constraint) spec
   = (c (StaticSymbol spec), c (FunctionSymbol spec), c (BlockSymbol spec))

type SerializeSymbolSpec spec
   = ( ConstraintSymbolSpec Eq spec
     , ConstraintSymbolSpec Hashable spec
     , ConstraintSymbolSpec Serialize spec)

data Module spec = Module
  { statics :: HM.HashMap (StaticSymbol spec) (Static spec)
  , functions :: HM.HashMap (FunctionSymbol spec) (Function spec)
  }

deriving instance
         ConstraintSymbolSpec Show spec => Show (Module spec)

deriving instance Generic (Module spec)

instance SerializeSymbolSpec spec => Serialize (Module spec)

data Static spec = Static
  { align :: Int
  , elements :: V.Vector (StaticElement spec)
  }

deriving instance
         ConstraintSymbolSpec Show spec => Show (Static spec)

deriving instance Generic (Static spec)

instance SerializeSymbolSpec spec => Serialize (Static spec)

data StaticElement spec
  = Uninitialized Int
  | BufferElement SBS.ShortByteString
  | StaticSymbolElement (StaticSymbol spec)
  | FunctionSymbolElement (FunctionSymbol spec)

deriving instance
         ConstraintSymbolSpec Show spec => Show (StaticElement spec)

deriving instance Generic (StaticElement spec)

instance SerializeSymbolSpec spec => Serialize (StaticElement spec)

data Function spec = Function
  { entryBlock :: BlockSymbol spec
  , blocks :: HM.HashMap (BlockSymbol spec) (Block spec)
  }

deriving instance
         ConstraintSymbolSpec Show spec => Show (Function spec)

deriving instance Generic (Function spec)

instance SerializeSymbolSpec spec => Serialize (Function spec)

data Block spec = Block
  { body :: Expression spec
  , branch :: Branch spec
  }

deriving instance
         ConstraintSymbolSpec Show spec => Show (Block spec)

deriving instance Generic (Block spec)

instance SerializeSymbolSpec spec => Serialize (Block spec)

data Expression spec =
  ExpressionStub

deriving instance
         ConstraintSymbolSpec Show spec => Show (Expression spec)

deriving instance Generic (Expression spec)

instance SerializeSymbolSpec spec => Serialize (Expression spec)

data Branch spec
  = CondBranch { cond :: Expression spec
               , trueDest, falseDest :: BlockSymbol spec }
  | SwitchBranch { switch :: Expression spec
                 , defDest :: BlockSymbol spec
                 , destMap :: HM.HashMap Word64 (BlockSymbol spec) }
  | CallBranch { callee :: Expression spec }

deriving instance
         ConstraintSymbolSpec Show spec => Show (Branch spec)

deriving instance Generic (Branch spec)

instance SerializeSymbolSpec spec => Serialize (Branch spec)

instance (Eq k, Hashable k, Serialize k, Serialize v) =>
         Serialize (HM.HashMap k v) where
  put = put . HM.toList
  get = HM.fromList <$> get

instance Serialize a => Serialize (V.Vector a) where
  put = put . V.toList
  get = V.fromList <$> get

instance Serialize SBS.ShortByteString where
  put = put . SBS.fromShort
  get = SBS.toShort <$> get
