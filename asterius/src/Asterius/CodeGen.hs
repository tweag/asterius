{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.CodeGen
  ( AsteriusStatics(..)
  , AsteriusModule(..)
  , chaseCLabel
  , marshalIR
  ) where

import qualified CLabel as GHC
import qualified Cmm as GHC
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Short as SBS
import Data.Data (Data)
import qualified Data.HashMap.Strict as HM
import Data.Serialize (Serialize)
import Data.String (fromString)
import Data.Traversable
import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified GhcPlugins as GHC
import Language.Haskell.GHC.Toolkit.Compiler
import Language.WebAssembly.NIR

data AsteriusStatic
  = Unresolved SBS.ShortByteString
  | Serialized SBS.ShortByteString
  deriving (Show, Generic, Data)

instance Serialize AsteriusStatic

newtype AsteriusStatics = AsteriusStatics
  { asteriusStatics :: V.Vector AsteriusStatic
  } deriving (Show, Generic, Data)

instance Serialize AsteriusStatics

data AsteriusModule = AsteriusModule
  { staticsMap :: HM.HashMap SBS.ShortByteString AsteriusStatics
  , functionMap :: HM.HashMap SBS.ShortByteString RelooperRun
  } deriving (Show, Generic, Data)

instance Serialize AsteriusModule

instance Semigroup AsteriusModule where
  AsteriusModule sm0 fm0 <> AsteriusModule sm1 fm1 =
    AsteriusModule (sm0 <> sm1) (fm0 <> fm1)

instance Monoid AsteriusModule where
  mempty = AsteriusModule mempty mempty

{-# INLINEABLE marshalCLabel #-}
marshalCLabel :: GHC.DynFlags -> GHC.CLabel -> SBS.ShortByteString
marshalCLabel dflags =
  fromString . GHC.showSDoc dflags . GHC.pprCode GHC.AsmStyle . GHC.ppr

{-# INLINEABLE chaseCLabel #-}
chaseCLabel :: GHC.CLabel -> Maybe GHC.Module
chaseCLabel = GHC.nameModule_maybe <=< GHC.hasHaskellName

marshalCmmData ::
     MonadIO m
  => GHC.DynFlags
  -> GHC.Section
  -> GHC.CmmStatics
  -> m AsteriusStatics
marshalCmmData _ _ _ = pure $ AsteriusStatics []

marshalCmmProc :: MonadIO m => GHC.DynFlags -> GHC.CmmGraph -> m RelooperRun
marshalCmmProc _ _ = pure $ RelooperRun mempty mempty 0

marshalCmmDecl ::
     MonadIO m
  => GHC.DynFlags
  -> GHC.GenCmmDecl GHC.CmmStatics h GHC.CmmGraph
  -> m AsteriusModule
marshalCmmDecl dflags decl =
  case decl of
    GHC.CmmData s d@(GHC.Statics clbl _) -> do
      ss <- marshalCmmData dflags s d
      pure $ AsteriusModule [(marshalCLabel dflags clbl, ss)] mempty
    GHC.CmmProc _ clbl _ g -> do
      r <- marshalCmmProc dflags g
      pure $ AsteriusModule mempty [(marshalCLabel dflags clbl, r)]

marshalIR :: MonadIO m => GHC.DynFlags -> IR -> m AsteriusModule
marshalIR dflags IR {..} = fmap mconcat $ for cmmRaw $ marshalCmmDecl dflags
