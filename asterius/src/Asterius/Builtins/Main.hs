{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.Main
  ( mainBuiltins,
  )
where

import Asterius.Types
import Asterius.Types.EntitySymbolMap
-- import qualified Data.Map.Strict as M

mainBuiltins :: AsteriusModule
mainBuiltins =
  mempty
    { ffiMarshalState =
        mempty
          { ffiExportDecls =
              unitESM
                "main"
                FFIExportDecl
                  { ffiFunctionType =
                      FFIFunctionType
                        { ffiParamTypes = [],
                          ffiResultTypes = [],
                          ffiInIO = True
                        },
                    ffiExportClosure = "Main_main_closure"
                  }
          }
    }
