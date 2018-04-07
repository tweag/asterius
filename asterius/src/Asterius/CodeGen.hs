{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.CodeGen
  ( AsteriusCodeGenError(..)
  , AsteriusStatics(..)
  , AsteriusModule(..)
  , chaseCLabel
  , marshalIR
  ) where

import qualified CLabel as GHC
import qualified Cmm as GHC
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Short as SBS
import Data.Data (Data)
import qualified Data.HashMap.Strict as HM
import Data.Int
import Data.Serialize (Serialize)
import Data.String (fromString)
import Data.Traversable
import qualified Data.Vector as V
import Data.Word
import GHC.Generics (Generic)
import qualified GhcPlugins as GHC
import qualified Hoopl.Block as GHC
import qualified Hoopl.Graph as GHC
import qualified Hoopl.Label as GHC
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.Orphans.Show ()
import Language.WebAssembly.Internals
import Language.WebAssembly.NIR

newtype AsteriusCodeGenError =
  UnsupportedCmmLit GHC.CmmLit
  deriving (Show)

instance Exception AsteriusCodeGenError

data AsteriusStatic
  = Unresolved SBS.ShortByteString
  | UnresolvedOff SBS.ShortByteString
                  Int
  | Uninitialized Int
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

{-#INLINEABLE marshalLabel#-}
marshalLabel :: GHC.DynFlags -> GHC.Label -> SBS.ShortByteString
marshalLabel dflags =
  marshalCLabel dflags . GHC.mkLocalBlockLabel . GHC.getUnique

{-# INLINEABLE chaseCLabel #-}
chaseCLabel :: GHC.CLabel -> Maybe GHC.Module
chaseCLabel = GHC.nameModule_maybe <=< GHC.hasHaskellName

marshalCmmStatic ::
     MonadIO m => GHC.DynFlags -> GHC.CmmStatic -> m AsteriusStatic
marshalCmmStatic dflags st =
  liftIO $
  case st of
    GHC.CmmStaticLit lit ->
      case lit of
        GHC.CmmInt x GHC.W8 ->
          pure $
          Serialized $
          if x < 0
            then encodePrim (fromIntegral x :: Int8)
            else encodePrim (fromIntegral x :: Word8)
        GHC.CmmInt x GHC.W16 ->
          pure $
          Serialized $
          if x < 0
            then encodePrim (fromIntegral x :: Int16)
            else encodePrim (fromIntegral x :: Word16)
        GHC.CmmInt x GHC.W32 ->
          pure $
          Serialized $
          if x < 0
            then encodePrim (fromIntegral x :: Int32)
            else encodePrim (fromIntegral x :: Word32)
        GHC.CmmInt x GHC.W64 ->
          pure $
          Serialized $
          if x < 0
            then encodePrim (fromIntegral x :: Int64)
            else encodePrim (fromIntegral x :: Word64)
        GHC.CmmFloat x GHC.W32 ->
          pure $ Serialized $ encodePrim (fromRational x :: Float)
        GHC.CmmFloat x GHC.W64 ->
          pure $ Serialized $ encodePrim (fromRational x :: Double)
        GHC.CmmLabel clbl ->
          pure $ Asterius.CodeGen.Unresolved $ marshalCLabel dflags clbl
        GHC.CmmLabelOff clbl o ->
          pure $ UnresolvedOff (marshalCLabel dflags clbl) o
        _ -> throwIO $ UnsupportedCmmLit lit
    GHC.CmmUninitialised s -> pure $ Uninitialized s
    GHC.CmmString s -> pure $ Serialized $ SBS.pack $ s ++ [0]

marshalCmmData ::
     MonadIO m
  => GHC.DynFlags
  -> GHC.Section
  -> GHC.CmmStatics
  -> m AsteriusStatics
marshalCmmData dflags _ (GHC.Statics _ ss) =
  fmap (AsteriusStatics . V.fromList) $ for ss $ marshalCmmStatic dflags

marshalCmmBlock ::
     MonadIO m
  => GHC.DynFlags
  -> (SBS.ShortByteString, [GHC.CmmNode GHC.O GHC.O], GHC.CmmNode GHC.O GHC.C)
  -> m (SBS.ShortByteString, RelooperBlock)
marshalCmmBlock _ (k, _, _) =
  pure (k, RelooperBlock {addBlock = AddBlock Null, addBranches = []})

marshalCmmProc :: MonadIO m => GHC.DynFlags -> GHC.CmmGraph -> m RelooperRun
marshalCmmProc dflags GHC.CmmGraph {g_graph = GHC.GMany _ body _, ..} = do
  rbs <-
    fmap HM.fromList $
    for
      [ (marshalLabel dflags k, GHC.blockToList inner_nodes, exit_node)
      | (k, GHC.BlockCC _ inner_nodes exit_node) <- GHC.bodyList body
      ] $
    marshalCmmBlock dflags
  pure
    RelooperRun
      {entry = marshalLabel dflags g_entry, blockMap = rbs, labelHelper = 233}

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
