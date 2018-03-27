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
  ( IRSpec(..)
  , ConstraintSymbolSpec
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

class ( ConstraintSymbolSpec Show spec
      , ConstraintSymbolSpec Eq spec
      , ConstraintSymbolSpec Hashable spec
      , ConstraintSymbolSpec Serialize spec
      ) =>
      IRSpec spec
  where
  type ModuleSymbol spec
  type EntrySymbol spec
  type BlockSymbol spec

type ConstraintSymbolSpec (c :: Type -> Constraint) spec
   = (c (ModuleSymbol spec), c (EntrySymbol spec), c (BlockSymbol spec))

data Module spec = Module
  { statics :: HM.HashMap (EntrySymbol spec) (Static spec)
  , functions :: HM.HashMap (EntrySymbol spec) (Function spec)
  }

instance IRSpec spec => Semigroup (Module spec) where
  {-# INLINE (<>) #-}
  m0 <> m1 =
    Module
      { statics = statics m0 <> statics m1
      , functions = functions m0 <> functions m1
      }

instance IRSpec spec => Monoid (Module spec) where
  {-# INLINE mempty #-}
  mempty = Module {statics = mempty, functions = mempty}

deriving instance IRSpec spec => Show (Module spec)

deriving instance Generic (Module spec)

instance IRSpec spec => Serialize (Module spec)

data Static spec = Static
  { align :: Int
  , elements :: V.Vector (StaticElement spec)
  }

deriving instance IRSpec spec => Show (Static spec)

deriving instance Generic (Static spec)

instance IRSpec spec => Serialize (Static spec)

data StaticElement spec
  = Uninitialized Int
  | BufferElement SBS.ShortByteString
  | SymbolElement (EntrySymbol spec)
  | SymbolOffElement (EntrySymbol spec)
                     Int

deriving instance IRSpec spec => Show (StaticElement spec)

deriving instance Generic (StaticElement spec)

instance IRSpec spec => Serialize (StaticElement spec)

data Function spec = Function
  { entryBlock :: BlockSymbol spec
  , blocks :: HM.HashMap (BlockSymbol spec) (Block spec)
  }

deriving instance IRSpec spec => Show (Function spec)

deriving instance Generic (Function spec)

instance IRSpec spec => Serialize (Function spec)

data Block spec = Block
  { body :: Expression spec
  , branch :: Branch spec
  }

deriving instance IRSpec spec => Show (Block spec)

deriving instance Generic (Block spec)

instance IRSpec spec => Serialize (Block spec)

data Expression spec =
  ExpressionStub

deriving instance IRSpec spec => Show (Expression spec)

deriving instance Generic (Expression spec)

instance IRSpec spec => Serialize (Expression spec)

data Branch spec
  = CondBranch { cond :: Expression spec
               , trueDest, falseDest :: BlockSymbol spec }
  | SwitchBranch { switch :: Expression spec
                 , defDest :: BlockSymbol spec
                 , destMap :: HM.HashMap Word64 (BlockSymbol spec) }
  | CallBranch { callee :: Expression spec }

deriving instance IRSpec spec => Show (Branch spec)

deriving instance Generic (Branch spec)

instance IRSpec spec => Serialize (Branch spec)

instance (Eq k, Hashable k, Serialize k, Serialize v) =>
         Serialize (HM.HashMap k v) where
  {-# INLINE put #-}
  put = put . HM.toList
  {-# INLINE get #-}
  get = HM.fromList <$> get

instance Serialize a => Serialize (V.Vector a) where
  {-# INLINE put #-}
  put v = put (V.length v) *> V.mapM_ put v
  {-# INLINE get #-}
  get = do
    len <- get
    V.replicateM len get

instance Serialize SBS.ShortByteString where
  {-# INLINE put #-}
  put sbs = put (SBS.length sbs) *> putShortByteString sbs
  {-# INLINE get #-}
  get = get >>= getShortByteString
