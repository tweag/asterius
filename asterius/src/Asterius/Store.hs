{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Store
  ( builtinsStore
  , addModule
  ) where

import Asterius.Builtins
import Asterius.Containers
import Asterius.Resolve
import Asterius.Types

{-# INLINEABLE builtinsStore #-}
builtinsStore :: BuiltinsOptions -> AsteriusStore
builtinsStore opts =
  addModule
    rtsAsteriusModuleSymbol
    (resolveGlobalRegs $ rtsAsteriusModule opts)
    AsteriusStore {symbolMap = mempty, moduleMap = mempty}

{-# INLINEABLE addModule #-}
addModule ::
     AsteriusModuleSymbol -> AsteriusModule -> AsteriusStore -> AsteriusStore
addModule mod_sym m@AsteriusModule {..} AsteriusStore {..} =
  AsteriusStore
    { symbolMap =
        hashMapUnions
          [ f staticsMap
          , f staticsErrorMap
          , f functionMap
          , f functionErrorMap
          , symbolMap
          ]
    , moduleMap = hashMapInsert mod_sym m moduleMap
    }
  where
    f = fmap (const mod_sym)
