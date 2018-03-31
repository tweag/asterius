{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Asterius.IR
  ( ModSym(..)
  , EntrySym(..)
  , BlockSym(..)
  , AsteriusIR
  , MarshalError(..)
  , marshalIR
  ) where

import qualified CLabel as GHC
import qualified Cmm as GHC
import Control.Monad.Except
import qualified Data.ByteString.Short as SBS
import Data.Hashable
import Data.Int
import Data.Serialize
import Data.String
import Data.Traversable
import Data.Word
import GHC.Exts
import GHC.Generics
import qualified Hoopl.Block as GHC
import qualified Hoopl.Collections as GHC
import qualified Hoopl.Graph as GHC
import qualified Hoopl.Label as GHC
import qualified HscTypes as GHC
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.Orphans.Show ()
import Language.WebAssembly.IR
import Language.WebAssembly.Internals
import qualified Module as GHC
import qualified Unique as GHC
import UnliftIO.Foreign

newtype ModSym =
  ModSym SBS.ShortByteString

deriving instance Show ModSym

deriving instance Eq ModSym

deriving instance Generic ModSym

instance Hashable ModSym

instance Serialize ModSym

data EntrySym =
  EntrySym ModSym
           SBS.ShortByteString

deriving instance Show EntrySym

deriving instance Eq EntrySym

deriving instance Generic EntrySym

instance Hashable EntrySym

instance Serialize EntrySym

data BlockSym =
  BlockSym ModSym
           EntrySym
           Int

deriving instance Show BlockSym

deriving instance Eq BlockSym

deriving instance Generic BlockSym

instance Hashable BlockSym

instance Serialize BlockSym

data AsteriusIR

instance IRSpec AsteriusIR where
  type ModuleSymbol AsteriusIR = ModSym
  type EntrySymbol AsteriusIR = EntrySym
  type BlockSymbol AsteriusIR = BlockSym

data MarshalError =
  UnsupportedStaticElement ModSym
                           GHC.CmmLit

deriving instance Show MarshalError

marshalIR ::
     MonadError MarshalError m => GHC.ModSummary -> IR -> m (Module AsteriusIR)
marshalIR GHC.ModSummary {..} IR {..} =
  fmap mconcat $
  for cmmRaw $
  marshalRawCmmDecl $ ModSym $ fromString $ GHC.moduleStableString ms_mod

marshalRawCmmDecl ::
     MonadError MarshalError m
  => ModSym
  -> GHC.RawCmmDecl
  -> m (Module AsteriusIR)
marshalRawCmmDecl mod_sym decl =
  case decl of
    GHC.CmmData _ (GHC.Statics static_sym ss) ->
      marshalCmmData mod_sym static_sym ss
    GHC.CmmProc _ func_sym _ graph -> marshalCmmProc mod_sym func_sym graph

marshalCmmData ::
     MonadError MarshalError m
  => ModSym
  -> GHC.CLabel
  -> [GHC.CmmStatic]
  -> m (Module AsteriusIR)
marshalCmmData mod_sym static_sym ss = do
  ses <- for ss $ marshalCmmStatic mod_sym
  pure
    mempty
      { statics =
          [ ( EntrySym mod_sym (encodeCLabel static_sym)
            , Static {align = 8, elements = fromList ses})
          ]
      }

marshalCmmStatic ::
     MonadError MarshalError m
  => ModSym
  -> GHC.CmmStatic
  -> m (StaticElement AsteriusIR)
marshalCmmStatic mod_sym static_rec =
  case static_rec of
    GHC.CmmStaticLit lit ->
      case lit of
        GHC.CmmInt i GHC.W8 ->
          pure $
          BufferElement $
          if i < 0
            then encodeStorable (fromIntegral i :: Int8)
            else encodeStorable (fromIntegral i :: Word8)
        GHC.CmmInt i GHC.W16 ->
          pure $
          BufferElement $
          if i < 0
            then encodeStorable (fromIntegral i :: Int16)
            else encodeStorable (fromIntegral i :: Word16)
        GHC.CmmInt i GHC.W32 ->
          pure $
          BufferElement $
          if i < 0
            then encodeStorable (fromIntegral i :: Int32)
            else encodeStorable (fromIntegral i :: Word32)
        GHC.CmmInt i GHC.W64 ->
          pure $
          BufferElement $
          if i < 0
            then encodeStorable (fromIntegral i :: Int64)
            else encodeStorable (fromIntegral i :: Word64)
        GHC.CmmFloat f GHC.W32 ->
          pure $ BufferElement $ encodeStorable (fromRational f :: CFloat)
        GHC.CmmFloat f GHC.W64 ->
          pure $ BufferElement $ encodeStorable (fromRational f :: CDouble)
        GHC.CmmLabel cl ->
          pure $ SymbolElement $ EntrySym mod_sym $ encodeCLabel cl
        GHC.CmmLabelOff cl offset ->
          pure $ SymbolOffElement (EntrySym mod_sym $ encodeCLabel cl) offset
        _ -> throwError $ UnsupportedStaticElement mod_sym lit
    GHC.CmmUninitialised len -> pure $ Uninitialized len
    GHC.CmmString ws -> pure $ BufferElement $ SBS.pack ws <> "\0"

marshalCmmProc ::
     MonadError MarshalError m
  => ModSym
  -> GHC.CLabel
  -> GHC.CmmGraph
  -> m (Module AsteriusIR)
marshalCmmProc mod_sym func_sym GHC.CmmGraph {..} = do
  bs <-
    for (GHC.mapToList g_body) $ \(l, b) -> do
      r <- marshalCmmBlock mod_sym entry_sym b
      pure (encodeLabel mod_sym entry_sym l, r)
  pure
    mempty
      { functions =
          [ ( entry_sym
            , Function
                { entryBlock = encodeLabel mod_sym entry_sym g_entry
                , blocks = fromList bs
                })
          ]
      }
  where
    entry_sym = EntrySym mod_sym $ encodeCLabel func_sym
    GHC.GMany GHC.NothingO g_body GHC.NothingO = g_graph

marshalCmmBlock ::
     MonadError MarshalError m
  => ModSym
  -> EntrySym
  -> GHC.Block GHC.CmmNode GHC.C GHC.C
  -> m (Block AsteriusIR)
marshalCmmBlock mod_sym func_sym (GHC.BlockCC (GHC.CmmEntry _ _) proc_nodes exit_node) = do
  e <- marshalCmmInstrs proc_nodes
  br <- marshalCmmBranch mod_sym func_sym exit_node
  pure Block {body = e, branch = br}

marshalCmmInstrs ::
     MonadError MarshalError m
  => GHC.Block GHC.CmmNode GHC.O GHC.O
  -> m (Expression AsteriusIR)
marshalCmmInstrs _ = pure ExpressionStub

marshalCmmBranch ::
     MonadError MarshalError m
  => ModSym
  -> EntrySym
  -> GHC.CmmNode GHC.O GHC.C
  -> m (Branch AsteriusIR)
marshalCmmBranch mod_sym func_sym exit_node =
  case exit_node of
    GHC.CmmBranch l -> pure UncondBranch {dest = encodeLabel mod_sym func_sym l}
    GHC.CmmCondBranch {..} ->
      pure
        CondBranch
          { cond = ExpressionStub
          , trueDest = encodeLabel mod_sym func_sym cml_true
          , falseDest = encodeLabel mod_sym func_sym cml_false
          }
    GHC.CmmSwitch _ _ ->
      pure
        SwitchBranch {switch = ExpressionStub, defDest = Nothing, destMap = []}
    GHC.CmmCall {..} -> pure CallBranch {callee = ExpressionStub}
    GHC.CmmForeignCall {} -> pure ForeignCallStub

encodeCLabel :: GHC.CLabel -> SBS.ShortByteString
encodeCLabel = fromString . show

encodeLabel :: ModSym -> EntrySym -> GHC.Label -> BlockSym
encodeLabel mod_sym entry_sym =
  BlockSym mod_sym entry_sym . GHC.getKey . GHC.getUnique
