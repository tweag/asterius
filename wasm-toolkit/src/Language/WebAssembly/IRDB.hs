{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.WebAssembly.IRDB
  ( ModuleMap(..)
  , SanCheckException(..)
  , addModule
  ) where

import Control.Monad.Except
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

data SanCheckException spec =
  SanCheckException

deriving instance
         ConstraintSymbolSpec Show spec => Show (SanCheckException spec)

addModule ::
     (MonadError (SanCheckException spec) m, HashSymbolSpec spec)
  => ModuleMap spec
  -> ModuleSymbol spec
  -> Module spec
  -> m (ModuleMap spec)
addModule mm mod_name mod_rec = do
  sanCheckStatics mm mod_name mod_rec
  sanCheckFunctions mm mod_name mod_rec
  pure ModuleMap {unModuleMap = HM.insert mod_name mod_rec $ unModuleMap mm}

sanCheckStatics ::
     MonadError (SanCheckException spec) m
  => ModuleMap spec
  -> ModuleSymbol spec
  -> Module spec
  -> m ()
sanCheckStatics = undefined

sanCheckFunctions ::
     MonadError (SanCheckException spec) m
  => ModuleMap spec
  -> ModuleSymbol spec
  -> Module spec
  -> m ()
sanCheckFunctions = undefined
