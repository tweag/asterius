{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Store
  ( builtinsStore
  , addModule
  ) where

import Asterius.Builtins
import Asterius.Types
import qualified Data.Map.Lazy as LM
import qualified Data.Map.Strict as M
import Prelude hiding (IO)

builtinsStore :: BuiltinsOptions -> AsteriusStore
builtinsStore opts =
  addModule rtsAsteriusModuleSymbol (rtsAsteriusModule opts) mempty

addModule ::
     AsteriusModuleSymbol -> AsteriusModule -> AsteriusStore -> AsteriusStore
addModule mod_sym m@AsteriusModule {..} AsteriusStore {..} =
  AsteriusStore
    { symbolMap =
        M.unions
          [ f staticsMap
          , f staticsErrorMap
          , f functionMap
          , f functionErrorMap
          , symbolMap
          ]
    , moduleMap = LM.insert mod_sym m moduleMap
    }
  where
    f = fmap (const mod_sym)
