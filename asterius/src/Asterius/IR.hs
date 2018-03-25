{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Asterius.IR
  ( ModSym(..)
  , StaticSym(..)
  , FuncSym(..)
  , BlockSym(..)
  , AsteriusIR
  , marshalIR
  ) where

import qualified Data.ByteString.Short as SBS
import Data.Hashable
import Data.Serialize
import qualified GHC
import GHC.Generics
import Language.Haskell.GHC.Toolkit.Compiler
import Language.WebAssembly.IR

newtype ModSym =
  ModSym SBS.ShortByteString

deriving instance Show ModSym

deriving instance Eq ModSym

deriving instance Generic ModSym

instance Hashable ModSym

instance Serialize ModSym

data StaticSym =
  StaticSym ModSym
            SBS.ShortByteString

deriving instance Show StaticSym

deriving instance Eq StaticSym

deriving instance Generic StaticSym

instance Hashable StaticSym

instance Serialize StaticSym

data FuncSym =
  FuncSym ModSym
          SBS.ShortByteString

deriving instance Show FuncSym

deriving instance Eq FuncSym

deriving instance Generic FuncSym

instance Hashable FuncSym

instance Serialize FuncSym

data BlockSym =
  BlockSym FuncSym
           Int

deriving instance Show BlockSym

deriving instance Eq BlockSym

deriving instance Generic BlockSym

instance Hashable BlockSym

instance Serialize BlockSym

data AsteriusIR

instance IRSpec AsteriusIR where
  type ModuleSymbol AsteriusIR = ModSym
  type StaticSymbol AsteriusIR = StaticSym
  type FunctionSymbol AsteriusIR = FuncSym
  type BlockSymbol AsteriusIR = BlockSym

marshalIR :: GHC.ModSummary -> IR -> Module AsteriusIR
marshalIR = undefined
