{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.WebAssembly.IRDB
  ( ModuleMap(..)
  , SanCheckErrorContext(..)
  , SanCheckError(..)
  , addModule
  ) where

import Control.Monad.Except
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.Serialize
import GHC.Generics
import Language.WebAssembly.IR

newtype ModuleMap spec = ModuleMap
  { unModuleMap :: HM.HashMap (ModuleSymbol spec) (Module spec)
  }

deriving instance
         ConstraintSymbolSpec Show spec => Show (ModuleMap spec)

deriving instance Generic (ModuleMap spec)

instance SerializeSymbolSpec spec => Serialize (ModuleMap spec)

data SanCheckErrorContext spec =
  SanCheckErrorContext (ModuleMap spec)
                       (ModuleSymbol spec)
                       (Module spec)

deriving instance
         ConstraintSymbolSpec Show spec => Show (SanCheckErrorContext spec)

data SanCheckError spec
  = StaticAlignmentError (SanCheckErrorContext spec)
                         (StaticSymbol spec)
  | UninitializedStaticError (SanCheckErrorContext spec)
                             (StaticSymbol spec)
  | UnresolvedStaticSymbolError (SanCheckErrorContext spec)
                                (StaticSymbol spec)
  | UnresolvedFunctionSymbolError (SanCheckErrorContext spec)
                                  (FunctionSymbol spec)
  | UnresolvedEntryBlockError (SanCheckErrorContext spec)
                              (FunctionSymbol spec)

deriving instance
         ConstraintSymbolSpec Show spec => Show (SanCheckError spec)

addModule ::
     (MonadError (SanCheckError spec) m, HashSymbolSpec spec)
  => ModuleMap spec
  -> ModuleSymbol spec
  -> Module spec
  -> m (ModuleMap spec)
addModule mod_map mod_sym mod_rec = do
  sanCheckStatics mod_map mod_sym mod_rec
  sanCheckFunctions mod_map mod_sym mod_rec
  pure ModuleMap {unModuleMap = HM.insert mod_sym mod_rec $ unModuleMap mod_map}

sanCheckStatics ::
     (MonadError (SanCheckError spec) m, HashSymbolSpec spec)
  => ModuleMap spec
  -> ModuleSymbol spec
  -> Module spec
  -> m ()
sanCheckStatics mod_map mod_sym mod_rec =
  for_ (HM.toList $ statics mod_rec) $
  uncurry $ sanCheckStatic mod_map mod_sym mod_rec

sanCheckStatic ::
     (MonadError (SanCheckError spec) m, HashSymbolSpec spec)
  => ModuleMap spec
  -> ModuleSymbol spec
  -> Module spec
  -> StaticSymbol spec
  -> Static spec
  -> m ()
sanCheckStatic mod_map mod_sym mod_rec static_name static_rec = do
  let cxt = SanCheckErrorContext mod_map mod_sym mod_rec
  unless (align static_rec >= 1) $
    throwError $ StaticAlignmentError cxt static_name
  for_ (elements static_rec) $
    sanCheckStaticElement mod_map mod_sym mod_rec static_name

sanCheckStaticElement ::
     (MonadError (SanCheckError spec) m, HashSymbolSpec spec)
  => ModuleMap spec
  -> ModuleSymbol spec
  -> Module spec
  -> StaticSymbol spec
  -> StaticElement spec
  -> m ()
sanCheckStaticElement mod_map mod_sym mod_rec static_name static_element = do
  let cxt = SanCheckErrorContext mod_map mod_sym mod_rec
  case static_element of
    Uninitialized size ->
      unless (size >= 0) $ throwError $ UninitializedStaticError cxt static_name
    StaticSymbolElement sym -> sanCheckStaticSymbol mod_map mod_sym mod_rec sym
    FunctionSymbolElement sym ->
      sanCheckFunctionSymbol mod_map mod_sym mod_rec sym
    _ -> pure ()

sanCheckFunctions ::
     (MonadError (SanCheckError spec) m, HashSymbolSpec spec)
  => ModuleMap spec
  -> ModuleSymbol spec
  -> Module spec
  -> m ()
sanCheckFunctions mod_map mod_sym mod_rec =
  for_ (HM.toList $ functions mod_rec) $
  uncurry $ sanCheckFunction mod_map mod_sym mod_rec

sanCheckFunction ::
     (MonadError (SanCheckError spec) m, HashSymbolSpec spec)
  => ModuleMap spec
  -> ModuleSymbol spec
  -> Module spec
  -> FunctionSymbol spec
  -> Function spec
  -> m ()
sanCheckFunction mod_map mod_sym mod_rec func_sym func_rec = do
  let cxt = SanCheckErrorContext mod_map mod_sym mod_rec
  unless (HM.member (entryBlock func_rec) (blocks func_rec)) $
    throwError $ UnresolvedEntryBlockError cxt func_sym
  sanCheckBlocks mod_map mod_sym mod_rec func_sym func_rec

sanCheckBlocks ::
     (MonadError (SanCheckError spec) m, HashSymbolSpec spec)
  => ModuleMap spec
  -> ModuleSymbol spec
  -> Module spec
  -> FunctionSymbol spec
  -> Function spec
  -> m ()
sanCheckBlocks mod_map mod_sym mod_rec func_sym func_rec =
  for_ (HM.toList $ blocks func_rec) $
  uncurry $ sanCheckBlock mod_map mod_sym mod_rec func_sym func_rec

sanCheckBlock ::
     (MonadError (SanCheckError spec) m, HashSymbolSpec spec)
  => ModuleMap spec
  -> ModuleSymbol spec
  -> Module spec
  -> FunctionSymbol spec
  -> Function spec
  -> BlockSymbol spec
  -> Block spec
  -> m ()
sanCheckBlock mod_map mod_sym mod_rec func_sym func_rec block_sym block_rec = do
  sanCheckExpression
    mod_map
    mod_sym
    mod_rec
    func_sym
    func_rec
    block_sym
    block_rec
    (body block_rec)
  sanCheckBranch mod_map mod_sym mod_rec func_sym func_rec block_sym block_rec

sanCheckExpression ::
     (MonadError (SanCheckError spec) m, HashSymbolSpec spec)
  => ModuleMap spec
  -> ModuleSymbol spec
  -> Module spec
  -> FunctionSymbol spec
  -> Function spec
  -> BlockSymbol spec
  -> Block spec
  -> Expression spec
  -> m ()
sanCheckExpression _ _ _ _ _ _ _ _ = pure ()

sanCheckBranch ::
     (MonadError (SanCheckError spec) m, HashSymbolSpec spec)
  => ModuleMap spec
  -> ModuleSymbol spec
  -> Module spec
  -> FunctionSymbol spec
  -> Function spec
  -> BlockSymbol spec
  -> Block spec
  -> m ()
sanCheckBranch _ _ _ _ _ _ _ = pure ()

sanCheckStaticSymbol ::
     (MonadError (SanCheckError spec) m, HashSymbolSpec spec)
  => ModuleMap spec
  -> ModuleSymbol spec
  -> Module spec
  -> StaticSymbol spec
  -> m ()
sanCheckStaticSymbol mod_map mod_sym mod_rec static_sym =
  unless
    (HM.member static_sym (statics mod_rec) ||
     any (HM.member static_sym . statics) (unModuleMap mod_map)) $
  throwError $
  UnresolvedStaticSymbolError
    (SanCheckErrorContext mod_map mod_sym mod_rec)
    static_sym

sanCheckFunctionSymbol ::
     (MonadError (SanCheckError spec) m, HashSymbolSpec spec)
  => ModuleMap spec
  -> ModuleSymbol spec
  -> Module spec
  -> FunctionSymbol spec
  -> m ()
sanCheckFunctionSymbol mod_map mod_sym mod_rec func_sym =
  unless
    (HM.member func_sym (functions mod_rec) ||
     any (HM.member func_sym . functions) (unModuleMap mod_map)) $
  throwError $
  UnresolvedFunctionSymbolError
    (SanCheckErrorContext mod_map mod_sym mod_rec)
    func_sym
