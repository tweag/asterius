{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Asterius.Types
  ( AsteriusCodeGenError(..)
  , AsteriusStatic(..)
  , AsteriusStatics(..)
  , AsteriusFunction(..)
  , AsteriusModule(..)
  , AsteriusModuleSymbol(..)
  , AsteriusSymbolKind(..)
  , AsteriusSymbolInfo(..)
  , AsteriusSymbolDatabase(..)
  ) where

import Control.DeepSeq
import qualified Data.ByteString.Short as SBS
import Data.Data
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Serialize
import qualified Data.Vector as V
import GHC.Generics
import Language.WebAssembly.Types
import UnliftIO

data AsteriusCodeGenError
  = UnsupportedCmmLit SBS.ShortByteString
  | UnsupportedCmmInstr SBS.ShortByteString
  | UnsupportedCmmBranch SBS.ShortByteString
  | UnsupportedCmmType SBS.ShortByteString
  | UnsupportedCmmGlobalReg SBS.ShortByteString
  | UnsupportedCmmExpr SBS.ShortByteString
  | UnsupportedRelooperAddBlock RelooperAddBlock
  | UnsupportedImplicitCasting Expression
                               ValueType
                               ValueType
  | UnresolvedSymbols (HS.HashSet UnresolvedSymbol)
  | UnhandledException SBS.ShortByteString
  deriving (Show, Generic, Data)

instance Serialize AsteriusCodeGenError

instance NFData AsteriusCodeGenError

instance Exception AsteriusCodeGenError

data AsteriusStatic
  = UnresolvedStatic UnresolvedSymbol
  | UnresolvedOffStatic UnresolvedSymbol
                        Int
  | Uninitialized Int
  | Serialized SBS.ShortByteString
  deriving (Show, Generic, Data)

instance Serialize AsteriusStatic

instance NFData AsteriusStatic

newtype AsteriusStatics = AsteriusStatics
  { asteriusStatics :: V.Vector AsteriusStatic
  } deriving (Show, Generic, Data)

instance Serialize AsteriusStatics

instance NFData AsteriusStatics

newtype AsteriusFunction = AsteriusFunction
  { body :: RelooperRun
  } deriving (Show, Generic, Data)

instance Serialize AsteriusFunction

instance NFData AsteriusFunction

data AsteriusModule = AsteriusModule
  { staticsMap :: HM.HashMap UnresolvedSymbol AsteriusStatics
  , staticsErrorMap :: HM.HashMap UnresolvedSymbol AsteriusCodeGenError
  , functionMap :: HM.HashMap UnresolvedSymbol AsteriusFunction
  , functionErrorMap :: HM.HashMap UnresolvedSymbol AsteriusCodeGenError
  } deriving (Show, Generic, Data)

instance Serialize AsteriusModule

instance NFData AsteriusModule

instance Semigroup AsteriusModule where
  AsteriusModule sm0 se0 fm0 fe0 <> AsteriusModule sm1 se1 fm1 fe1 =
    AsteriusModule (sm0 <> sm1) (se0 <> se1) (fm0 <> fm1) (fe0 <> fe1)

instance Monoid AsteriusModule where
  mempty = AsteriusModule mempty mempty mempty mempty

data AsteriusModuleSymbol = AsteriusModuleSymbol
  { unitId :: SBS.ShortByteString
  , moduleName :: V.Vector SBS.ShortByteString
  } deriving (Show, Generic, Data)

instance Serialize AsteriusModuleSymbol

instance NFData AsteriusModuleSymbol

data AsteriusSymbolKind
  = StaticsSymbol
  | FunctionSymbol
  deriving (Show, Generic, Data)

instance Serialize AsteriusSymbolKind

instance NFData AsteriusSymbolKind

data AsteriusSymbolInfo = AsteriusSymbolInfo
  { symbolKind :: AsteriusSymbolKind
  , symbolSource :: AsteriusModuleSymbol
  } deriving (Show, Generic, Data)

instance Serialize AsteriusSymbolInfo

instance NFData AsteriusSymbolInfo

newtype AsteriusSymbolDatabase = AsteriusSymbolDatabase
  { symbolMap :: HM.HashMap UnresolvedSymbol AsteriusSymbolInfo
  } deriving (Show, Generic, Data)

instance Serialize AsteriusSymbolDatabase

instance NFData AsteriusSymbolDatabase
