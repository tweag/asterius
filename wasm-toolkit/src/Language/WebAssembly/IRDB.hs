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

deriving instance
         ConstraintSymbolSpec Show spec => Show (SanCheckError spec)

addModule ::
     (MonadError (SanCheckError spec) m, HashSymbolSpec spec)
  => ModuleMap spec
  -> ModuleSymbol spec
  -> Module spec
  -> m (ModuleMap spec)
addModule mm mod_name mod_rec = do
  sanCheckStatics mm mod_name mod_rec
  sanCheckFunctions mm mod_name mod_rec
  pure ModuleMap {unModuleMap = HM.insert mod_name mod_rec $ unModuleMap mm}

sanCheckStatics ::
     (MonadError (SanCheckError spec) m, HashSymbolSpec spec)
  => ModuleMap spec
  -> ModuleSymbol spec
  -> Module spec
  -> m ()
sanCheckStatics mm mod_name mod_rec =
  for_ (HM.toList $ statics mod_rec) $
  uncurry $ sanCheckStatic mm mod_name mod_rec

sanCheckStatic ::
     (MonadError (SanCheckError spec) m, HashSymbolSpec spec)
  => ModuleMap spec
  -> ModuleSymbol spec
  -> Module spec
  -> StaticSymbol spec
  -> Static spec
  -> m ()
sanCheckStatic mm mod_name mod_rec static_name static_rec = do
  let cxt = SanCheckErrorContext mm mod_name mod_rec
  unless (align static_rec >= 1) $
    throwError $ StaticAlignmentError cxt static_name
  for_ (elements static_rec) $
    sanCheckStaticElement mm mod_name mod_rec static_name

sanCheckStaticElement ::
     (MonadError (SanCheckError spec) m, HashSymbolSpec spec)
  => ModuleMap spec
  -> ModuleSymbol spec
  -> Module spec
  -> StaticSymbol spec
  -> StaticElement spec
  -> m ()
sanCheckStaticElement mm mod_name mod_rec static_name static_element = do
  let cxt = SanCheckErrorContext mm mod_name mod_rec
  case static_element of
    Uninitialized size ->
      unless (size >= 0) $ throwError $ UninitializedStaticError cxt static_name
    StaticSymbolElement sym -> sanCheckStaticSymbol mm mod_name mod_rec sym
    FunctionSymbolElement sym -> sanCheckFunctionSymbol mm mod_name mod_rec sym
    _ -> pure ()

sanCheckFunctions ::
     (MonadError (SanCheckError spec) m, HashSymbolSpec spec)
  => ModuleMap spec
  -> ModuleSymbol spec
  -> Module spec
  -> m ()
sanCheckFunctions = undefined

sanCheckStaticSymbol ::
     (MonadError (SanCheckError spec) m, HashSymbolSpec spec)
  => ModuleMap spec
  -> ModuleSymbol spec
  -> Module spec
  -> StaticSymbol spec
  -> m ()
sanCheckStaticSymbol mm mod_name mod_rec static_sym =
  unless
    (HM.member static_sym (statics mod_rec) ||
     any (HM.member static_sym . statics) (unModuleMap mm)) $
  throwError $
  UnresolvedStaticSymbolError
    (SanCheckErrorContext mm mod_name mod_rec)
    static_sym

sanCheckFunctionSymbol ::
     (MonadError (SanCheckError spec) m, HashSymbolSpec spec)
  => ModuleMap spec
  -> ModuleSymbol spec
  -> Module spec
  -> FunctionSymbol spec
  -> m ()
sanCheckFunctionSymbol mm mod_name mod_rec func_sym =
  unless
    (HM.member func_sym (functions mod_rec) ||
     any (HM.member func_sym . functions) (unModuleMap mm)) $
  throwError $
  UnresolvedFunctionSymbolError
    (SanCheckErrorContext mm mod_name mod_rec)
    func_sym
